;;;; liaison.asd

(asdf:defsystem #:liaison
  :serial t
  :description "Describe liaison here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:hunchentoot
               #:cl-who
               #:parenscript
               #:css-lite
               #:uuid
               #:jsown
               #:css-lite
               #:cl-ivy
               #:drakma
               #:postmodern
               #:net-telent-date)
  :components ((:file "package")
               (:module "src"
                        :components ((:file "db")
                                     (:file "macros")
                                     (:file "liaison")))))
