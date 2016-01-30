(in-package :cl-user)
(defpackage imp.db
  (:use :cl)
  ;; Generics
  (:export :display-name)
  ;; Variables
  (:export :*operating-systems*
           :*architectures*)
  ;; Classes
  (:export :implementation
           :implementation-name
           :implementation-display-name
           :implementation-versions
           :version
           :version-identifier
           :version-systems
           :system
           :system-os
           :system-arch
           :compatible-arch-p)
  ;; Interface
  (:export :*implementations*
           :find-implementation
           :register-implementation
           :remove-implementation
           :register-version
           :define-implementation
           :define-version
           :binary-exists-p
           :binary-url
           :download
           :installer-pathname
           :install
           :binary-pathname)
  ;; Errors
  (:export :imp-error
           :no-implementation
           :error-implementation
           :error-version
           :error-system)
  (:documentation "The database of known implementations."))
(in-package :imp.db)

;;; Generics

(defgeneric display-name (object)
  (:documentation "Return the user-viewable name of some named object."))

;;; Variables

(defparameter *operating-systems*
  (list :linux :osx :windows)
  "A list of supported operating system names.")

(defmethod display-name ((object (eql :linux))) "Linux")
(defmethod display-name ((object (eql :osx))) "OS X")
(defmethod display-name ((object (eql :windows))) "Windows")

(defparameter *architectures*
  (list :x86 :amd64 :arm :ppc)
  "A list of supported architecture names.")

(defmethod display-name ((object (eql :x86))) "32-bit")
(defmethod display-name ((object (eql :amd64))) "64-bit")
(defmethod display-name ((object (eql :arm))) "ARM")
(defmethod display-name ((object (eql :ppc))) "PPC")

(defun compatible-arch-p (system arch)
  "Is the application architecture compatible with the system's?"
  (or (eq system arch)
      (and (eq system :amd64)
           (eq arch :x86))))

;;; Classes

(defclass implementation ()
  ((name :reader implementation-name
         :initarg :name
         :type keyword
         :documentation "The name of the implementation, a keyword.")
   (display-name :reader implementation-display-name
                 :initarg :display-name
                 :type string
                 :documentation "The display name of the implementation.")
   (versions :accessor implementation-versions
             :initarg :versions
             :initform (list)
             :type list
             :documentation "A list of implementation versions.")
   (url-fn :reader implementation-url-fn
           :initarg :url-fn
           :type function
           :documentation "The function that returns the download URL for the
           implementation, given a version identifier, and OS and architecture
           identifiers."))
  (:documentation "An implementation."))

(defclass version ()
  ((identifier :reader version-identifier
               :initarg :identifier
               :type keyword
               :documentation "The version identifier as a keyboard,
               e.g. @c(:1.2.3).")
   (systems :reader version-systems
            :initarg :systems
            :initform (list)
            :type list
            :documentation "The list of systems this version is available for."))
  (:documentation "A particular version of an implementation."))

(defclass system ()
  ((os :reader system-os
       :initarg :os
       :type keyword
       :documentation "The operating system identifier.")
   (arch :reader system-arch
         :initarg :arch
         :type keyword
         :documentation "The architecture identifier."))
  (:documentation "A system is a pair of an OS identifier and architecture identifier."))

(defvar *implementations* (list)
  "A list of known implementations.")

;;; Methods

(defmethod display-name ((object implementation))
  (implementation-display-name object))

;;; Managing the DB

(defun find-implementation (name)
  "Find an implementation by keyword name."
  (find name *implementations* :key #'implementation-name :test #'eq))

(defun register-implementation (impl)
  "Add an implementation to the database."
  (unless (find-implementation (implementation-name impl))
    (push impl *implementations*))
  impl)

(defun remove-implementation (name)
  "Remove an implementation from the database."
  (delete name *implementations* :test #'(lambda (impl)
                                           (eq (implementation-name impl)
                                               name))))

(defun find-version (impl identifier)
  (find identifier (implementation-versions impl)
        :key #'version-identifier
        :test #'eq))

(defun register-version (impl-name version)
  "Register a version of the implementation."
  (let ((impl (find-implementation impl-name)))
    (when impl
      (unless (find-version impl (version-identifier version))
        (push version (implementation-versions impl)))))
  version)

(defmacro define-implementation (name &key display-name)
  `(register-implementation
    (make-instance 'implementation
                   :name ,name
                   :display-name ,display-name)))

(defmacro define-version (impl-name identifier &key systems)
  `(register-version
    ,impl-name
    (make-instance 'version
                   :identifier ,identifier
                   :systems (list
                             ,@(mapcar #'(lambda (system)
                                           (destructuring-bind (os arch)
                                               system
                                             `(make-instance 'system
                                                             :os ,os
                                                             :arch ,arch)))
                                       systems)))))

;;; Methods

(defgeneric binary-exists-p (name version os arch)
  (:documentation "Whether a binary of an implementation exists given its
  version identifier, and the target OS and architecture.")

  (:method ((name symbol) version os arch)
    "The default method, valid for all implementations."
    (declare (type keyword version os arch))
    (let ((impl (find-implementation name)))
      (if impl
          ;; There is an implementation with that name
          (loop for ver in (implementation-versions impl) do
            (when (eq version (version-identifier ver))
              ;; Found the version. Do any of the systems match?
              (loop for system in (version-systems ver) do
                (when (and (eq os (system-os system))
                           (compatible-arch-p arch (system-arch system)))
                  (return-from binary-exists-p t)))))
          ;; No such implementation
          nil))))

(defun ensure-binary-exists (name version os arch)
  "If the binary exists, return T. Otherwise, signal the appropriate error."
  (unless (binary-exists-p name version os arch)
    (error 'no-implementation
           :implementation name
           :version version
           :system (make-instance 'system
                                  :os os
                                  :arch arch))))

(defgeneric binary-url (name version os arch)
  (:documentation "Return the download URL for a particular implementation given
  its version identifier, and the target OS and architecture.")

  (:method :before ((name symbol) version os arch)
    "Ensure this implementation is available."
    (declare (type keyword version os arch))
    (ensure-binary-exists name version os arch)))

(defgeneric download (name version os arch directory)
  (:documentation "Download the implementation, specified by version, OS and
architecture, to a target directory. The target directory will be created if it
doesn't exist, and filled with the implementation's files.

If the implementation is distributed in an archive file, this is stored in a
temporary file and deleted upon extraction."))

(defgeneric needs-install-p (name version os arch)
  (:documentation "Does this version of this implementation need to be
  installed (T), or does the download include a directly executable
  binary (NIL)?"))

(defgeneric installer-pathname (name version os arch directory)
  (:documentation "Return the path to the implementation's installer in the
  directory where it was downloaded."))

(defgeneric install (name version os arch directory install-directory)
  (:documentation "Install an implementation that was downloaded to directory to
  the install-directory.")

  (:method :before ((name symbol) version os arch directory install-directory)
    "Pre-install checks."
    (ensure-directories-exist install-directory)))

(defgeneric binary-pathname (name version os arch install-directory)
  (:documentation "Return the path to the implementation's executable in the
  directory where it was installed."))

;;; Print methods

(defmethod print-object ((object implementation) stream)
  "Print an implementation object."
  (print-unreadable-object (object stream :type t)
    (format stream "~A (~D versions)"
            (display-name object)
            (length (implementation-versions object)))))

(defmethod print-object ((object system) stream)
  "Print a system object."
  (print-unreadable-object (object stream :type t)
    (format stream "~A/~A" (display-name (system-os object)) (display-name (system-arch object)))))

;;; Errors

(define-condition imp-error ()
  ()
  (:documentation "The base class of all errors."))

(define-condition no-implementation (imp-error)
  ((implementation :reader error-implementation
                   :initarg :implementation
                   :type keyword
                   :documentation "The implementation identifier.")
   (version :reader error-version
            :initarg :version
            :type keyword
            :documentation "The version identifier.")
   (system :reader error-system
           :initarg :system
           :type system
           :documentation "The system."))
  (:report
   (lambda (condition stream)
     (format stream "No download available: ~A version ~A is not available for ~A on ~A."
             (error-implementation condition)
             (error-version condition)
             (display-name (system-os (error-system condition)))
             (display-name (system-arch (error-system condition))))))
  (:documentation "Signalled when there exists no available download for the
  given version, OS and architecture. For example, an implementation would
  provide Windows binaries only for 32-bit and 64-bit systems, not for SPARC
  workstations."))
