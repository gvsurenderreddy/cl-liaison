;;;; liaison.lisp

(in-package #:liaison)

;;; "liaison" goes here. Hacks and glory await!

(defparameter *site-acceptor* nil)
(defparameter *dispatch-table* nil)

(db.use "liaison")

(setq hunchentoot:*show-lisp-errors-p* t
      *show-lisp-backtraces-p* t)
(setq hunchentoot:*dispatch-table*
      (list
       (hunchentoot:create-prefix-dispatcher "/login" 'handler/login)
       (hunchentoot:create-prefix-dispatcher "/register" 'handler/register)

       (hunchentoot:create-prefix-dispatcher "/beacon" 'ajax/beacon)
       (hunchentoot:create-prefix-dispatcher "/gather" 'ajax/load-map)

       (hunchentoot:create-prefix-dispatcher "/css" 'handler/site-css)

       (hunchentoot:create-regex-dispatcher "^/$" 'page/main)))

(defun srv/start (&key (port 8082))
  (labels ((resource-path (path)
             (truename (asdf:system-relative-pathname :liaison path))))
    (setq *site-acceptor*
          (make-instance 'hunchentoot:easy-acceptor
                         :document-root (resource-path "./resources/static/")
                         :port port))
    (hunchentoot:start *site-acceptor*)))
(defun srv/stop ()
  (hunchentoot:stop *site-acceptor*))

(defmacro w/session (&rest body)
  `(progn
     (hunchentoot:start-session)
     (setf (hunchentoot:session-max-time hunchentoot:*session*) (* 60 60 24 265))
     ,@body))
(defmacro w/logged-in (&rest body)
  `(let ((uid (hunchentoot:session-value :uid)))
     (if uid
         ,@body
         (hunchentoot:redirect "/login"))))
(defmacro @-q (collection query)
  `(docs (iter (db.find ,collection ,query :limit 0))))
(defmacro @-> (name)
  `(hunchentoot:parameter ,name))
(defmacro re/kill (pattern target)
  `(cl-ppcre:regex-replace-all ,pattern ,target ""))
(defmacro w/page (title &rest body)
  `(cl-who:with-html-output-to-string
       (*standard-output* nil :prologue nil :indent t)
     (:html :lang "en"
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
       (:title ,title)
       (:link :type "text/css" :rel "stylesheet" :href "/bs/css/bootstrap.css")
       (:link :type "text/css" :rel "stylesheet" :href "http://fonts.googleapis.com/css?family=Anonymous+Pro|Cantarell|Ubuntu|Ubuntu+Mono")
       (:link :type "text/css" :rel "stylesheet" :href "/pr/prettify.css")
       (:link :type "text/css" :rel "stylesheet" :href "/liaison.css")
;       (:link :type "text/css" :rel "stylesheet" :href "/inez.css")
       (:link :type "text/css" :rel "stylesheet" :href "/solarized.css")
       (:link :type "text/css" :rel "stylesheet" :href "/css")
       (:link :rel "shortcut icon" :href "/favicon.ico")
       (:link :rel "apple-touch-icon" :href "/bs/images/apple-touch-icon.png")
       (:link :rel "apple-touch-icon" :sizes "72x72" :href "/bs/images/apple-touch-icon-72x72.png")
       (:link :rel "apple-touch-icon" :sizes "114x114" :href "/bs/images/apple-touch-icon-114x114.png")
       (:script :type "text/javascript" :src "http://maps.google.com/maps/api/js?sensor=false&key=AIzaSyDsOVRkRfKm3kBVrUaih3xRPYp6dRe8iZ4")
       (:script :type "text/javascript" :src "/jquery-min.js")
       (:script :type "text/javascript" :src "/liaison.js")
     
      (:body :style "padding-top: 10px;"
             ,@body
             (dialog-msg)
             (htm
              (:div :class "navbar navbar-fixed-bottom"
               (:div :class "navbar-inner"
                 (:div :class "container"
                  (:ul :class "nav"
                    (if (w/session (hunchentoot:session-value :uid))
                        (htm
                         (:li (:a :href "/" "Home"))
                         (:li (:a :href "/logout" "Logout"))
                         (:li (:a :href "#"
                                  :onclick (str (ps
                                                  (liaison.beacon)))
                                  "Ping")))
                        (htm
                         (:li (:a :href "/" "Home"))
                         (:li (:a :href "/login" "Login"))))))))
              (htm
               (:script :src "/bs/js/bootstrap.js")
               (:script :src "/pr/prettify.js")
               (:script :src "/liaison.js"))))))))

(defun u/uid ()
  (car (w/session
        (hunchentoot:session-value :uid))))
(defun w/ajax (msg)
  (setf (hunchentoot:content-type*) "application/json")
  msg)
(defun unique-id ()
  (format nil "~a"
    (uuid:make-v4-uuid)))
(defun dialog-set (m)
  (w/session (setf (hunchentoot:session-value :status) m)))
(defun dialog-msg ()
  (w/session
    (let ((msg (hunchentoot:session-value :status)))
      (and msg
        (progn
          (hunchentoot:delete-session-value :status)
          (htm
           (:div :class "alert alert-danger"
                 (str msg))))))))
(defun check/email-exists (email)
  (@-q "users" ($ "email" email)))
(defun hash-password (pas)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256
    (ironclad:ascii-string-to-byte-array pas))))
(defun page/main ()
  (w/logged-in
   (w/page "Geo"
    (:div :class "container"
      (:div :class "row"
        (:script (str (ps ((@ ($ document) ready) (liaison.mapfire)
                           (liaison.beacon)))))
        (:div :id "status" "Loading...")
        (:div :class "span12 well" :id "canvas"))))))
(defun handler/login ()
  (labels ((login-page ()
             (w/page "Login"
                (:div :class "container"
                  (:div :class "row"
                    (:div :class "well"
                      (:form :method "POST"
                             :action "/login"
                             :class "form form-vertical"
                        (:label "Login")
                        (:input :type "text"
                                :class "span3 xinput-large"
                                :name "email")
                        (:label "Password")
                        (:input :type "password"
                                :class "span3"
                                :name "password")
                        (:div :class "form-actions"
                          (:button :type "submit"
                                   :class "btn btn-primary" "Login")))
                      (:a :href "/register"
                        (:button :href "/register"
                                 :class "btn" "Register")))))))
           (login-action ()
             (let* ((the-pass (@-> "password"))
                    (the-email (@-> "email"))
                    (the-doc (@-q "users" ($ ($ "email" the-email)
                                             ($ "password"
                                                (hash-password the-pass))))))
               (if the-doc
                   (w/session
                    (setf (hunchentoot:session-value :uid )
                          (get-element "uid" the-doc))
                    (dialog-set "Logged in!")
                    (hunchentoot:redirect "/"))
                   (progn
                     (dialog-set "Login incorrect. :(")
                     (hunchentoot:redirect "/login"))))))
    (if (eq (hunchentoot:request-method*) :POST)
        (login-action)
        (login-page))))
(defun handler/register ()
  (labels ((register-page ()
             (w/page "Register"
                (:div :class "well span6"
                  (:form :method "POST" :action "/register"
                    (htm
                     (:label "Email")
                     (:input :type "text" :class "span3" :name "email")
                     (:label "Sekret Coed")
                     (:input :type 'text :class "span3" :name "sekret")
                     (:label "Password")
                     (:input :type "password" :class "span3" :name "password")
                     (:div :class "form-actions"
                       (:button :type "submit"
                                :class "btn btn-primary" "Register")))))))
           (register-action ()
               (let ((new-email (@-> "email"))
                     (sekret (@-> "sekret"))
                     (new-pass (@-> "password")))
                 (cond ((and new-email new-pass)
                        (let ((td (make-document))
                              (uu (unique-id)))
                          (if (@-q "users" ($ "email" new-email))
                              (progn
                                (dialog-set "Another user exists with that email address.")
                                (hunchentoot:redirect "/register"))
                              (progn
                                (if (string= sekret "ingram")
                                    (progn
                                      (add-element "email" new-email td)
                                      (add-element "uid" uu td)
                                      (add-element "password" (hash-password new-pass) td)
                                      (add-element "realname" "Real Name" td)
                                      (db.save "users" td)
                                      (dialog-set "Registration successful!")
                                      (w/session
                                       (setf (hunchentoot:session-value "uid") uu)
                                       (hunchentoot:redirect "/login")))
                                    (progn
                                      (dialog-set "BAD SEKRET COED!!!")
                                      (hunchentoot:redirect "/register")))))))
                       (t (progn
                            (dialog-set "Something's horribly wrong.")
                            (hunchentoot:redirect "/register")))))))
    (if (eq (hunchentoot:request-method*) :POST)
        (register-action)
        (register-page))))
(defun action/register ()
(defun ajax/beacon ()
  (labels ((normalize-name (f)
             (re/kill "position"
                 (re/kill "coords"
                     (re/kill "\\]+|\\[+" f)))))
    (let* ((new-doc (make-document))
           (my-uid (w/session (u/uid)))
           (all-keys (hunchentoot:post-parameters*)))
      (if (< 0 (length all-keys))
          (progn
            (loop for (a . b) in all-keys
               do (progn
                    (or (string= b "null")
                        (add-element (normalize-name a) b new-doc))))
            (add-element "uid" my-uid new-doc)
            (add-element "timestamp" (cl-mongo:now) new-doc)
            (db.save "beacon" new-doc)
            (w/ajax "{result:'true'}"))
          (w/ajax "{result:'failed'}"))))))
(defun handler/site-css ()
  (setf (hunchentoot:content-type*) "text/css")
  (css-lite:css
    ((:body) (:margin-bottom "60px"))))
(defun ajax/load-map ()
  (let* ((cts (chronicity:parse "30 minutes ago"))
         (cts-sec (chronicity:sec-of cts))
         (cts-min (chronicity:minute-of cts))
         (cts-hour (chronicity:hour-of cts))
         (cts-day (chronicity:day-of cts))
         (cts-month (chronicity:month-of cts))
         (cts-year (chronicity:year-of cts))
         (mongo-timestamp (date-time cts-sec
                                     cts-min
                                     cts-hour
                                     cts-day
                                     cts-month
                                     cts-year))
         (people (docs (iter (db.find "beacon"
                                      ($ "timestamp"
                                         ($ "$gte" mongo-timestamp)))))))
    (w/ajax
     (jsown:to-json
      (mapcar #'(lambda (person)
                 (let ((po (empty-object)))
                   (setf (jsown:val po "latitude")
                         (get-element "latitude" person))
                   (setf (jsown:val po "longitude")
                         (get-element "longitude" person))
                   (setf (jsown:val po "uid")
                         (get-element "uid" person))))
             people)))))
