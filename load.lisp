
(ql:quickload 'swank)
(swank:create-server :port 4004 :dont-close t)

(ql:quickload 'liaison)
(liaison:srv/start :port 8084)
