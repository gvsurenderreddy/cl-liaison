;;;; liaison.asd

(asdf:defsystem #:liaison
  :serial t
  :description "Describe liaison here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:hunchentoot
               #:cl-who
               #:lparallel
               #:parenscript
               #:css-lite
               #:uuid
               #:chronicity
               #:jsown
               #:css-lite
               #:cl-mongo)
  :components ((:file "package")
               (:file "liaison")))

