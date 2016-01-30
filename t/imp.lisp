(in-package :cl-user)
(defpackage imp-test
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :imp-test)

(def-suite db
  :description "Imp DB tests.")
(in-suite db)

(defun url-exists-p (url)
  (handler-case
      (progn
        (dex:head url)
        t)
    (t () nil)))

(defmacro do-all-implementations ((name version os arch) &body body)
  (alexandria:with-gensyms (impl ver sys)
    `(loop for ,impl in imp.db:*implementations* do
      (loop for ,ver in (imp.db:implementation-versions ,impl) do
        (loop for ,sys in (imp.db:version-systems ,ver) do
          (with-accessors ((,os imp.db:system-os) (,arch imp.db:system-arch)) ,sys
            (let ((,name (imp.db:implementation-name ,impl))
                  (,version (imp.db:version-identifier ,ver)))
              ,@body)))))))

(defmacro do-implementations ((name version os arch) impls &body body)
  `(loop for (,name ,version ,os ,arch) in ',impls do
    ,@body))

(test downloads-are-valid
  (do-all-implementations (name version os arch)
    (is-true
     (imp.db:binary-exists-p name version os arch))
    (let ((url (imp.db:binary-url name version os arch)))
      (is-true
       (stringp url))
      (is
       (url-exists-p url)))))

(defun directory-empty-p (directory)
  (and (uiop:emptyp (uiop:directory-files directory))
       (uiop:emptyp (uiop:subdirectories directory))))

(test download
  (let ((dir (asdf:system-relative-pathname :imp #p"t/download/")))
    (do-implementations (name version os arch)
      ((:sbcl :1.2.16 :linux :amd64)
       (:sbcl :1.2.11 :osx :amd64)
       (:sbcl :1.2.16 :windows :amd64))
      (is-true
       (directory-empty-p dir))
      (finishes
        (imp.db:download name version os arch dir))
      (is-false
       (directory-empty-p dir))
      (is-true
       (probe-file
        (imp.db:installer-pathname name version os arch dir)))
      (finishes
        (uiop:delete-directory-tree dir :validate t)))))

(def-suite manager
  :description "Imp DB tests.")
(in-suite manager)

(defmacro with-manager ((manager) &body body)
  (let ((directory (gensym)))
    `(let* ((,directory (asdf:system-relative-pathname :imp #p"t/manager/"))
            (,manager (make-instance 'imp:manager
                                     :directory ,directory)))
       ,@body
       (finishes
         (uiop:delete-directory-tree ,directory :validate t)))))

(test basic
  (is (keywordp (imp::current-os)))
  (is (keywordp (imp::current-arch)))
  (is
   (not (uiop:emptyp (imp:available-implementations))))
  (is
   (not (uiop:emptyp (imp:available-versions :sbcl))))
  (with-manager (man)
    (is-false
     (probe-file (imp:database-path man)))
    (is
     (eql (length (imp:manager-implementations man))
          0))
    (finishes
      (imp:install man (make-instance 'imp:managed-implementation
                                      :name :sbcl
                                      :version :1.2.16)))
    (is
     (eql (length (imp:manager-implementations man))
          1))
    (finishes
      (imp:write-db man))
    (is-true
     (probe-file (imp:database-path man)))
    (finishes
      (imp:load-db man))
    (is
     (eql (length (imp:manager-implementations man))
          1))))

(test install-impls
  (with-manager (man)
    (flet ((test-impl (name version)
             (let ((impl (make-instance 'imp:managed-implementation
                                        :name name
                                        :version version)))
               (finishes
                 (imp:install man impl))
               (is-true
                (probe-file (imp:implementation-binary man impl))))))
      (loop for name in (imp:available-implementations) do
        (loop for version in (imp:available-versions name) do
          (test-impl name (imp.db:version-identifier version)))))))

(defun run-tests ()
  (run! 'db)
  (run! 'manager))
