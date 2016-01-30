(in-package :cl-user)
(defpackage imp
  (:use :cl)
  (:import-from :alexandria
                :compose)
  ;; Utilities
  (:export :available-implementations
           :available-versions)
  ;; Manager
  (:export :manager
           :manager-directory
           :manager-implementations
           :manager-downloads-directory
           :manager-implementations-directory
           :managed-implementation
           :implementation-name
           :implementation-version
           :implementation-download-directory
           :implementation-directory
           :available-implementations
           :available-versions
           :install
           :implementation-binary
           :database-path
           :write-db
           :load-db
           :managed-implementation
           :implementation-name
           :implementation-version
           :current-os
           :current-arch))
(in-package :imp)

;;; Utilities

(defun humanize (symbol)
  (declare (type symbol symbol))
  (string-downcase (symbol-name symbol)))

(defun current-os ()
  "Return the manager's underlying OS."
  (cond
    ((uiop:os-windows-p)
     :windows)
    ((uiop:os-macosx-p)
     :osx)
    ((uiop:os-unix-p)
     :linux)
    (t
     (error "Unknown operating system."))))

(defun current-arch ()
  "Return the manager's underlying architecture."
  (case (uiop:architecture)
    (:x64
     :amd64)
    (:x86
     :x86)
    (:ppc64
     :ppc)
    (:ppc32
     :ppc)
    (:arm
     :arm)
    (t
     (error "Unknown architecture."))))

(defun version-available (version)
  (declare (type imp.db:version version))
  "Given and a version object, return whether or not this version can be
installed in the current system."
  (when (find-if #'(lambda (system)
                     (and (eq (imp.db:system-os system) (current-os))
                          (imp.db:compatible-arch-p (current-arch)
                                                    (imp.db:system-arch system))))
                 (imp.db:version-systems version))
    t))

(defun available-implementations ()
  "Return a list of implementation identifiers."
  (mapcar #'imp.db:implementation-name imp.db:*implementations*))

(defun available-versions (implementation-name)
  "Given the name of an implementation, return a list of version identifiers
that can be installed."
  (declare (type keyword implementation-name))
  (assert (member implementation-name (available-implementations) :test #'eq))
  (remove-if (compose #'not #'version-available)
             (imp.db:implementation-versions
              (imp.db:find-implementation implementation-name))))

;;; Classes

(defclass manager ()
  ((directory :reader manager-directory
              :initarg :directory
              :type pathname
              :documentation "The directory that holds the downloaded
              implementations.")
   (implementations :accessor manager-implementations
                    :initarg :implementations
                    :initform (list)
                    :type list
                    :documentation "A list of installed implementations."))
  (:documentation "An implementation manager."))

(defclass managed-implementation ()
  ((name :reader implementation-name
         :initarg :name
         :type keyword
         :documentation "The implementation keyword identifier.")
   (version :reader implementation-version
            :initarg :version
            :type keyword
            :documentation "The implementation's version keywor identifier."))
  (:documentation "An installed implementation."))

(defmethod initialize-instance :after ((instance managed-implementation) &key)
  (with-slots (name version) instance
    (assert (member name (available-implementations) :test #'eq))
    (assert (member version (available-versions name) :key #'imp.db:version-identifier))))

;;; Directories

(defun manager-downloads-directory (manager)
  "The absolute pathname to the manager's downloads directory."
  (declare (type manager manager))
  (merge-pathnames #p"downloads/" (manager-directory manager)))

(defun manager-implementations-directory (manager)
  "The absolute pathname to the manager's implementations directory."
  (declare (type manager manager))
  (merge-pathnames #p"implementations/" (manager-directory manager)))

(defun implementation-directories (impl)
  "The (relative) tree of directories where this implementation will be stored."
  (declare (type managed-implementation impl))
  (parse-namestring
   (format nil "~A/~A/~A/~A/"
           (humanize (current-os))
           (humanize (current-arch))
           (humanize (implementation-name impl))
           (humanize (implementation-version impl)))))

(defun implementation-download-directory (manager impl)
  "Return the absolute pathname to the directory in the manager where the
implementation will be donwloaded."
  (declare (type manager manager)
           (type managed-implementation impl))
  (merge-pathnames (implementation-directories impl)
                   (manager-downloads-directory manager)))

(defun implementation-directory (manager impl)
  "Return the absolute pathname to the directory in the manager where the
implementation will be installed."
  (declare (type manager manager)
           (type managed-implementation impl))
  (merge-pathnames (implementation-directories impl)
                   (manager-implementations-directory manager)))

;;; Methods

(defun ensure-manager-directory-exists (manager)
  (declare (type manager manager))
  (ensure-directories-exist (manager-directory manager))
  nil)

(defmethod install (manager impl)
  "Install an implementation."
  (declare (type manager manager)
           (type managed-implementation impl))
  (with-slots (name version) impl
    (format t "~%Installing ~A ~A"
            (imp.db:display-name (imp.db:find-implementation name))
            (symbol-name version))
    (let ((download-dir (implementation-download-directory manager impl))
          (os (current-os))
          (arch (current-arch)))
      (format t "~%Downloading...")
      (imp.db:download name version os arch download-dir)
      (format t "~%Installing...")
      (imp.db:install name version os arch download-dir
                      (implementation-directory manager impl))
      (push impl (manager-implementations manager)))
    impl))

(defmethod implementation-binary (manager impl)
  "Return the path to the binary executable."
  (declare (type manager manager)
           (type managed-implementation impl))
  (with-slots (name version) impl
    (imp.db:binary-pathname name
                            version
                            (current-os)
                            (current-arch)
                            (implementation-directory manager impl))))

;;; Database

(defmethod database-path (manager)
  "Return the path to the database file."
  (declare (type manager manager))
  (merge-pathnames #p"manager.json"
                   (manager-directory manager)))

(defmethod write-db (manager)
  "Write the database of installs to the database file."
  (declare (type manager manager))
  (ensure-manager-directory-exists manager)
  (with-open-file (stream (database-path manager)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (yason:with-output (stream :indent t)
      (yason:with-object ()
        (yason:with-object-element ("implementations")
          (yason:with-array ()
            (loop for impl in (manager-implementations manager) do
              (yason:with-object ()
                (yason:Encode-object-elements
                 "name" (symbol-name (implementation-name impl))
                 "version" (symbol-name (implementation-version impl))))))))))
  manager)

(defmethod load-db (manager)
  "Load the stored data for this manager. This clears and overwrites the list of
  installed implementations."
  (declare (type manager manager))
  (let ((data (yason:parse (database-path manager))))
    (setf (manager-implementations manager)
          (mapcar #'(lambda (table)
                      (make-instance 'managed-implementation
                                     :name (intern (gethash "name" table) :keyword)
                                     :version (intern (gethash "version" table) :keyword)))
                  (gethash "implementations" data))))
  manager)
