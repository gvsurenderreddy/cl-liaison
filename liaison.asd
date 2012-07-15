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
               #:cl-moore
               #:drakma
               #:net-telent-date
               #:cl-mongo)
  :components ((:file "package")
               (:file "liaison")))

