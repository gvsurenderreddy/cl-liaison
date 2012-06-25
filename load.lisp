
(ql:quickload 'linedit)
(ql:quickload 'swank)
(ql:quickload 'liaison)
(swank:create-server :port 4004 :dont-close t)
(liaison:srv/start :port 8084)
