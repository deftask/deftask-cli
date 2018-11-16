(asdf:defsystem "deftask-cli"
  :description "deftask Command Line"
  :version "0.0.1"
  :author "Chaitanya Gupta <mail@chaitanyagupta.com>"
  :serial t
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "alexandria" "drakma" "cl-json" "unix-opts" "quri" "cl-interpol" "cl-ppcre" "termcolor")
  :components ((:file "sys-package")
               (:cffi-grovel-file "sys-grovel")
               (:file "utils")
               (:file "deftask")
               (:file "deftask-cli")))
