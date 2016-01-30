(defsystem imp-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:imp
               :fiveam
               :dexador
               :alexandria)
  :components ((:module "t"
                :serial t
                :components
                ((:file "imp")))))
