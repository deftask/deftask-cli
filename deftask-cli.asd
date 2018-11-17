(asdf:defsystem "deftask-cli"
  :description "deftask Command Line"
  :version "0.0.1"
  :author "Chaitanya Gupta <mail@chaitanyagupta.com>"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "alexandria" "drakma" "cl-json" "unix-opts" "quri" "cl-interpol" "cl-ppcre" "termcolor")
  :serial t
  :components ((:module "sys"
                        :components ((:file "package")
                                     (:cffi-grovel-file "grovel")
                                     (:file "defs")))
               (:file "utils")
               (:file "deftask")
               (:file "deftask-cli")))
