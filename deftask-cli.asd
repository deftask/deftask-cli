(asdf:defsystem "deftask-cli"
  :description "deftask Command Line"
  :version "0.0.1"
  :author "Chaitanya Gupta <mail@chaitanyagupta.com>"
  :serial t
  :depends-on ("alexandria" "drakma" "cl-json" "unix-opts" "quri" "cl-interpol" "cl-ppcre" "cl-ansi-term")
  :components ((:file "deftask")
               (:file "deftask-cli")))
