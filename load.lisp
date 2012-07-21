
(ql:quickload 'swank)
(swank:create-server :port 4084 :dont-close t)

(ql:quickload 'liaison)
(liaison:srv/start :port 8084)

(defparameter *whoami* 'liaison)
