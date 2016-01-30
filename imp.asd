(defsystem imp
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :depends-on (:trivial-download
               :trivial-extract
               :trivial-exe
               :trivial-msi
               :copy-directory
               :yason)
  :components ((:module "src"
                :serial t
                :components
                ((:file "db")
                 (:file "imp")
                 (:file "predefined"))))
  :description "A Lisp implementation downloader and manager."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op imp-test))))
