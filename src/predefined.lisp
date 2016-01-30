(in-package :imp.db)

;;; SBCL

(define-implementation :sbcl
  :display-name "SBCL")

;; Methods

(defun sbcl-directory-name (version os arch)
  (let ((version (symbol-name version))
        (arch (ccase arch
                (:x86 "x86")
                (:amd64 "x86-64")
                (:arm "armhf"))))
    (ccase os
      (:windows
       (format nil "sbcl-~A-~A-windows"
               version
               arch))
      (:linux
       (format nil "sbcl-~A-~A-linux"
               version
               arch))
      (:osx
       (format nil "sbcl-~A-~A-darwin"
               version
               arch)))))5

(defun sbcl-extension (os)
  (ccase os
    (:windows
     ".msi")
    (:linux
     ".tar.bz2")
    (:osx
     ".tar.bz2")))

(defmethod binary-url ((name (eql :sbcl)) version os arch)
  "Return the download URL for an SBCL binary."
  (concatenate 'string
               "http://ufpr.dl.sourceforge.net/project/sbcl/sbcl/"
               (symbol-name version)
               "/"
               (sbcl-directory-name version os arch)
               "-binary"
               (sbcl-extension os)))

(defmethod download :before (name version os arch directory)
  "Pre-download checks."
  (ensure-binary-exists name version os arch)
  (ensure-directories-exist directory))

(defun extract-tar.bz2 (pathname directory)
  (ensure-directories-exist directory)
  (uiop:run-program
   (format nil "tar -xvjf ~S -C ~S" (namestring pathname) (namestring directory))))

(defmethod download ((name (eql :sbcl)) version os arch directory)
  "Download SBCL on Linux or Mac. Both these systems usually have tar."
  (uiop:with-temporary-file (:pathname pathname)
    (trivial-download:download (binary-url name version os arch) pathname
                               :quiet t)
    (extract-tar.bz2 pathname directory))
  directory)

(defmethod download ((name (eql :sbcl)) version (os (eql :windows)) arch directory)
  "Download SBCL on Windows. This is just an MSI file."
  ;; Download the MSI file
  (trivial-download:download (binary-url name version os arch)
                             (merge-pathnames #p"installer.msi" directory)
                             :quiet t)
  directory)

(defmethod installer-pathname ((name (eql :sbcl)) version os arch directory)
  "Return the path to the SBCL installer executable."
  (merge-pathnames
   #p"install.sh"
   (merge-pathnames (parse-namestring
                     (concatenate 'string
                                  (sbcl-directory-name version os arch)
                                  "/"))
                    directory)))

(defmethod installer-pathname ((name (eql :sbcl)) version (os (eql :windows)) arch
                               directory)
  "Return the path to the SBCL MSI installer."
  (merge-pathnames #p"installer.msi" directory))

(defmethod install ((name (eql :sbcl)) version os arch directory install-directory)
  "Install SBCL on Linux or Mac."
  (let ((installer (installer-pathname name version os arch directory)))
    (trivial-exe:ensure-executable installer)
    (uiop:with-current-directory ((uiop:pathname-directory-pathname installer))
      (uiop:run-program
       (format nil "unset SBCL_HOME; INSTALL_ROOT=~S sh ~S"
               (namestring install-directory)
               (namestring installer))
       :force-shell t)))
  install-directory)

(defmethod install ((name (eql :sbcl)) version (os (eql :windows)) arch directory
                    install-directory)
  "Install SBCL on Windows."
  (let ((installer (installer-pathname name version os arch directory)))
    (trivial-msi:install installer
                         :directory install-directory
                         :mode :passive)))

(defmethod binary-pathname ((name (eql :sbcl)) version os arch install-directory)
  "The binary pathname in Linux and OS X."
  (merge-pathnames #p"bin/sbcl" install-directory))

;; Versions

(define-version :sbcl :1.2.16
  :systems ((:windows :x86)
            (:windows :amd64)
            (:linux :amd64)))

(define-version :sbcl :1.2.15
  :systems ((:windows :x86)
            (:windows :amd64)
            (:linux :amd64)))

(define-version :sbcl :1.2.14
  :systems ((:windows :x86)
            (:windows :amd64)
            (:linux :amd64)
            (:linux :arm)))

(define-version :sbcl :1.2.13
  :systems ((:windows :x86)
            (:windows :amd64)
            (:linux :amd64)))

(define-version :sbcl :1.2.12
  :systems ((:windows :x86)
            (:windows :amd64)
            (:linux :amd64)))

(define-version :sbcl :1.2.11
  :systems ((:windows :x86)
            (:windows :amd64)
            (:linux :amd64)
            (:osx :amd64)))

(define-version :sbcl :1.2.10
  :systems ((:linux :amd64)))

(define-version :sbcl :1.2.9
  :systems ((:linux :amd64)))

(define-version :sbcl :1.2.8
  :systems ((:linux :amd64)))

(define-version :sbcl :1.2.7
  :systems ((:windows :x86)
            (:windows :amd64)
            (:linux :x86)
            (:linux :amd64)))

(define-version :sbcl :1.2.6
  :systems ((:windows :amd64)
            (:linux :amd64)))

(define-version :sbcl :1.2.5
  :systems ((:linux :amd64)))

(define-version :sbcl :1.2.4
  :systems ((:linux :amd64)))

(define-version :sbcl :1.2.3
  :systems ((:linux :amd64)))

(define-version :sbcl :1.2.2
  :systems ((:linux :amd64)))

(define-version :sbcl :1.2.1
  :systems ((:windows :x86)
            (:windows :amd64)
            (:linux :amd64)))

;;; CCL

(define-implementation :ccl
  :display-name "CCL")

;; Download URL

(defmethod binary-url ((name (eql :ccl)) version os arch)
  "Return the download URL for an SBCL binary."
  (let ((filename (format nil "ccl-~A-~A~A~A"
                          (symbol-name version)
                          (case os
                            (:linux "linux")
                            (:windows "windows")
                            (:osx "darwin"))
                          (case arch
                            (:amd64 "x86")
                            (:x86 "x86")
                            (:ppc "ppc")
                            (:arm "arm"))
                          (if (eq os :windows)
                              ".zip"
                              ".tar.gz"))))
    (concatenate 'string
                 "http://ccl.clozure.com/ftp/pub/release/"
                 (symbol-name version)
                 "/"
                 filename)))

(defmethod binary-url ((name (eql :ccl)) (version (eql :1.4)) (os (eql :windows)) arch)
  "Special case."
  "http://ccl.clozure.com/ftp/pub/release/1.4/ccl-1.4-windows.zip")

(defun extract-tar.gz (pathname directory)
  (ensure-directories-exist directory)
  (uiop:run-program
   (format nil "tar -xvzf ~S -C ~S" (namestring pathname) (namestring directory))))

(defmethod download ((name (eql :ccl)) version os arch directory)
  "Download CCL on any operating system."
  (let* ((extraction-directory (merge-pathnames #p"extract/"
                                                (uiop:temporary-directory)))
         (pathname (merge-pathnames #p"archive" extraction-directory)))
    (trivial-download:download (binary-url name version os arch)
                               pathname
                               :quiet t)
    (if (eq os :windows)
        ;; Windows archives are .zip
        (trivial-extract:extract-zip pathname)
        (extract-tar.gz pathname extraction-directory))
    (copy-directory:copy (merge-pathnames #p"ccl/" extraction-directory) directory))
  directory)

(defmethod install ((name (eql :ccl)) version os arch directory install-directory)
  "Install CCL."
  (copy-directory:copy directory install-directory))

(defmethod binary-pathname ((name (eql :ccl)) version os arch install-directory)
  "The CCL binary pathname."
  (merge-pathnames #p"wx86cl64" install-directory))

(defmethod binary-pathname ((name (eql :ccl))
                            version
                            (os (eql :linux))
                            (arch (eql :amd64))
                            install-directory)
  (merge-pathnames #p"lx86cl64" install-directory))

(defmethod binary-pathname ((name (eql :ccl))
                            version
                            (os (eql :linux))
                            (arch (eql :x86))
                            install-directory)
  (merge-pathnames #p"lx86cl" install-directory))

(defmethod binary-pathname ((name (eql :ccl))
                            version
                            (os (eql :windows))
                            (arch (eql :amd64))
                            install-directory)
  (merge-pathnames #p"wx86cl64.exe" install-directory))

(defmethod binary-pathname ((name (eql :ccl))
                            version
                            (os (eql :windows))
                            (arch (eql :x86))
                            install-directory)
  (merge-pathnames #p"wx86cl.exe" install-directory))

;; Versions

(define-version :ccl :1.10
  :systems ((:windows :x86)
            (:linux :x86)
            (:linux :arm)
            (:linux :ppc)
            (:osx :x86)))

(define-version :ccl :1.9
  :systems ((:windows :x86)
            (:linux :x86)
            (:linux :arm)
            (:linux :ppc)
            (:osx :x86)))

(define-version :ccl :1.8
  :systems ((:windows :x86)
            (:linux :x86)
            (:linux :arm)
            (:linux :ppc)
            (:osx :x86)))

(define-version :ccl :1.7
  :systems ((:windows :x86)
            (:linux :x86)
            (:linux :arm)
            (:linux :ppc)
            (:osx :x86)))

(define-version :ccl :1.6
  :systems ((:windows :x86)
            (:linux :x86)
            (:linux :arm)
            (:linux :ppc)
            (:osx :x86)))

(define-version :ccl :1.5
  :systems ((:windows :x86)
            (:linux :x86)
            (:linux :ppc)
            (:osx :x86)))

(define-version :ccl :1.4
  :systems ((:windows :x86)
            (:linux :x86)
            (:linux :ppc)
            (:osx :x86)))

(define-version :ccl :1.4
  :systems ((:windows :x86)
            (:linux :x86)
            (:linux :ppc)
            (:osx :x86)))
