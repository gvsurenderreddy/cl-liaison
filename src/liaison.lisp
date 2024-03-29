
(in-package #:cl-liaison)

(defparameter *site-acceptor* nil)
(defparameter *dispatch-table* nil)

(setq hunchentoot:*show-lisp-errors-p* t
      *show-lisp-backtraces-p* t)

(setq hunchentoot:*dispatch-table*
      (list
       (hunchentoot:create-prefix-dispatcher "/login" 'handler/login)
       (hunchentoot:create-prefix-dispatcher "/logout" 'handler/logout)
       (hunchentoot:create-prefix-dispatcher "/register" 'handler/register)

       (hunchentoot:create-prefix-dispatcher "/profile" 'handler/profile)
       (hunchentoot:create-prefix-dispatcher "/settings/toggle" 'handler/settings)
       (hunchentoot:create-prefix-dispatcher "/settings/get/" 'handler/preference)

       (hunchentoot:create-prefix-dispatcher "/feeds" 'handler/feeds)

       (hunchentoot:create-prefix-dispatcher "/beacon" 'ajax/beacon)
       (hunchentoot:create-prefix-dispatcher "/gather" 'ajax/gather) ;load-public-map)
       (hunchentoot:create-prefix-dispatcher "/marker/" 'ajax/marker-info)

       (hunchentoot:create-prefix-dispatcher "/css" 'handler/site-css)
       (hunchentoot:create-prefix-dispatcher "/js" 'handler/site-js)
       (hunchentoot:create-regex-dispatcher "^/$" 'page/main)))

(defun srv/start (&key (port 8082))
  (setq *site-acceptor*
        (make-instance 'hunchentoot:easy-acceptor
                       :document-root (cl-ivy:resource-path "./resources/static/")
                       :port port))
  (hunchentoot:start *site-acceptor*))

(defun srv/stop ()
  (hunchentoot:stop *site-acceptor*))

(defpsmacro fsubmit (name)
  `(((@ $ each) ((@ ($ ,name) find) "input")) (lambda (i x)
                                                (let ((nam (@ x name))
                                                      (val (@ x value)))
                                                  '(nam val)))))
(defpsmacro settings-toggle ()
  "Used mainly for checkboxes, click the box, set the option in the database."
  `((@ $ ajax) (create
                 type "POST"
                 url "/settings/toggle"
                 data (create name ((@ ($ this) attr) "name")
                              value ((@ ($ this) attr) "checked")))))

(defpsmacro $sel (x)
  (concatenate 'string "#" x))

(defun page/main ()
  (no-cache)
  (cl-who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent nil)
    (:html :lang "en"
           (:head
            (:meta :charset "utf-8")
            (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
            (:title "Liaison")
            (:link :type "text/css" :rel "stylesheet" :href "/bs/css/bootstrap.css")
            (:link :type "text/css" :rel "stylesheet" :href "/bs/css/bootstrap-responsive.css")
            (:link :type "text/css" :rel "stylesheet" :href "https://fonts.googleapis.com/css?family=Anonymous+Pro|Cantarell|Ubuntu|Ubuntu+Mono")
            (:link :type "text/css" :rel "stylesheet" :href "/css")
            (:link :rel "shortcut icon" :href "/favicon.ico")
            (:link :rel "apple-touch-icon" :href "/bs/images/apple-touch-icon.png")
            (:link :rel "apple-touch-icon" :sizes "72x72" :href "/bs/images/apple-touch-icon-72x72.png")
            (:link :rel "apple-touch-icon" :sizes "114x114" :href "/bs/images/apple-touch-icon-114x114.png")
            (:script :type "text/javascript" :src "/jquery-min.js")
            (:script :src "/js")
            (:script :src "/bs/js/bootstrap.js")
            (:script :type "text/javascript"
                     :src "https://maps.google.com/maps/api/js?sensor=false&key=AIzaSyDsOVRkRfKm3kBVrUaih3xRPYp6dRe8iZ4")
            (:script :type "text/javascript"
                     (str (ps (var goog_markers ([]))
                              (var goog_map nil))))
            (:script :type "text/javascript"
                     (str (ps ((@ ($ document) ready)
                               (lambda ()
                                 ((@ ($ "#canvas") css) "margin-top" "45px")))))))
           (:body
            (:div :class "navbar navbar-fixed-top"
                  (:div :class "navbar-inner"
                        (:div :class "container"
                              (if (w/session (session-value :uid))
                                  (htm (:ul :class "nav"
                                            (:li :class "dropdown"
                                                 (:a :href "#"
                                                     :class "dropdown-toggle brand"
                                                     :data-toggle "dropdown"
                                                     "Liaison")
                                                 (:ul :class "dropdown-menu"
                                                      (:li :class "nav-header" "People")
                                                      (:li :class "nav-divider")
                                                      (:li (:a :href "#"
                                                               :onclick (ps (set-Timeout (lambda ()
                                                                                           ((@ liaison recenter)))
                                                                                         200)
                                                                            ((@ goog_map set-Zoom) 17)
                                                                            false)
                                                               "Find Me"))
                                                      (:li (:a :href "#"
                                                               :onclick (ps-inline ((@ liaison loader) 30)
                                                                                   false)
                                                               "Active within 30 minutes"))
                                                      (:li :class "nav-divider")
                                                      (:li :class "nav-header" "Options")
                                                      (:li :class "nav-divider")
                                                      (:li (:a :data-toggle "modal"
                                                               :href "#pmodal"
                                                               "Preferences"))
                                                      (:li :class "nav-header" "Map Options")
                                                      (:li :class "nav-divider")
                                                      (:li (:a :href "#"
                                                               :onclick (ps-inline ((@ liaison beacon))
                                                                                   false)
                                                               "Ping"))
                                                      (:li (:a :href "#"
                                                               :onclick (ps-inline ((@ liaison clearmap))
                                                                                   false)
                                                               "Clear The Map"))
                                                      (:li (:a :href "/"
                                                               "Reload The Page")))))
                                       (:ul :class "nav pull-right"
                                            (:li (:p :class "navbar-text" :id "tstatus"))
                                            (:li (:a :href "/logout" "Logout"))))
                                  
                                  (htm
                                   (:ul :class "nav"
                                        (:a :href "/" :class "brand" "Liaison")
                                        (:li (:a :href "/login" "Login"))))))))
            
            (:div :class "container"
                  (:div :class "row"
                        (:div :class "span12"
                              (:div :class "well" :id "canvas" "Honka..."))))
            (:div :id "pmodal" :class "modal hide fade"
                  (:div :class "modal-header"
                        (:button :type "button" :class "close" :data-dismiss "modal" "x")
                        (:h4 "Preferences"))
                  (:div :id "tester" :class "modal-body"
                        (:div :class "accordion" :id "cpanel"
                              (%-agroup :name "Profile"
                                        :dataparent "#cpanel"
                                        :inner (:form :method "POST"
                                                      :action "/profile"
                                                      :id "prefform"
                                                      :onsubmit (ps false)
                                                      :class "form-horizontal"
                                                      (%-cgroup :name "Public"
                                                                :targetid "profpub"
                                                                :inner (:span
                                                                        (:input :id "public"
                                                                                :type "checkbox"
                                                                                :name "public"
                                                                                :onclick (ps-inline (settings-toggle)))
                                                                        (:p :class "help-block"
                                                                            "If checked, your profile is publicly viewable to people who ARE NOT yet logged in.")))
                                                      (%-cgroup :name "Searchable"
                                                                :inner (:span
                                                                        (:input :id "search"
                                                                                :type "checkbox"
                                                                                :name "search"
                                                                                :onclick (ps-inline (settings-toggle)))
                                                                        (:p :class "help-block"
                                                                            "If checked, your profile is searchable by other users.  Note that users who are not logged in CANNOT search at all.")))))
                              (%-agroup :name "Developer Console"
                                        :dataparent "#cpanel"
                                        :inner (:form :id "devform" :class "form-horizontal"
                                                      (%-cgroup
                                                       :name "Development Mode"
                                                       :inner (:span (:input :id "developer"
                                                                             :type "checkbox"
                                                                             :name "developer"
                                                                             :onclick (ps-inline (settings-toggle)))
                                                                     (:p :class "help-block" "If checked, disables the map, among other things."))))))))
            (dialog-msg)))))

(defun u/uid ()
  (w/session
   (session-value :uid)))
(defun w/json (msg)
  (setf (hunchentoot:content-type*) "application/json")
  msg)
(defun dialog-set (message)
  (w/session (setf (session-value :status) message)))
(defun dialog-msg ()
  (w/session
    (let ((msg (session-value :status)))
      (and msg
        (progn
          (hunchentoot:delete-session-value :status)
          (cl-who:htm (:div :class "alert alert-danger" (str msg))))))))
(defun check/email-exists (email)
  (@-q "users" ($ "email" email)))

(defun hash-password (pas)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256
    (ironclad:ascii-string-to-byte-array pas))))

(defun unique-id ()
  (format nil "~a"
    (uuid:make-v4-uuid)))

(defun handler/login ()
  (labels ((login-page ()
             (with-plain-page
                 (:div :class "container"
                       (:div :class "span6 center offset2"
                             (:form :class "form-signin" :method "POST" :action "/login"
                                    (:h2 :class "form-signin-heading" "Please sign in")
                                    (:input :type "text" :class "input-block-level" :placeholder "Email Address")
                                    (:input :type "password" :class "input-block-level" :placeholder "Password")
                                    (:button :class "btn btn-large btn-primary" :type "submit" "Sign In")
                                    (:a :class "btn btn-large" :href "/register" "Register"))))))
           (login-action ()
             (let* ((the-pass (parameter "password"))
                    (the-email (parameter "email"))
                    (the-doc (car (with-pg
                                    (query (:select '* :from 'customer :where (:and (:= 'password (hash-password the-pass))
                                                                                    (:= 'email the-email))))))))
               (if the-doc
                   (w/session
                    (setf (session-value :uid )
                          (customer-uid the-doc))
                    (hunchentoot:redirect "/"))
                   (progn
                     (dialog-set "Login incorrect. :(")
                     (hunchentoot:redirect "/login"))))))
    (if (eq (hunchentoot:request-method*) :POST)
        (login-action)
        (login-page))))

(defun handler/logout ()
  (w/session
   (hunchentoot:delete-session-value :uid))
  (hunchentoot:redirect "/"))

(defun handler/register ()
  (labels ((register-page ()
             (with-plain-page
                 (:div :class "container"
                       (:div :class "span6 center offset2"
                             (:form :method "POST" :action "/register" :class "form-signin"
                                    (:h2 :class "form-signin-heading" "Register")
                                    (:input :name "email" :type "text" :class "input-block-level" :placeholder "Email Address")
                                    (:input :name "password" :type "password" :class "input-block-level" :placeholder "Password")
                                    (:button :class "btn btn-large btn-primary" :type "submit" "Register"))))))
           (register-action ()
             (let ((email (parameter "email"))
                   (password (parameter "password")))
               
               (cond ((with-pg (query (:select 'uid :from 'customer :where (:= 'email email))))
                      (progn (dialog-set "There is already a customer with that email address.")
                             (redirect "/register")))
                     ((and (< 3 (length email)) (< 3 (length password)))
                      (let ((new-customer (make-instance 'customer
                                                         :email email
                                                         :password (hash-password password))))
                        (with-pg (insert-dao new-customer))
                        (dialog-set "Registration succesful!")
                        (w/session (setf (session-value :uid) (customer-uid new-customer)))
                        (redirect "/")))
                     (t (progn
                          (dialog-set "Something went horribly wrong.")
                          (redirect "/register")))))))
    (if (eq (hunchentoot:request-method*) :POST)
        (register-action)
        (register-page))))

(defun handler/site-css ()
  (setf (hunchentoot:content-type*) "text/css")
  (css-lite:css
    (("*") (:font-family "Open Sans,Ubuntu,arial"))
    (("html,body") (:height "100%"))
    ((".container,.row,#canvas,.span12") (:height "inherit"))))

(defun ajax/gather ()
  (no-cache)
  (let* ((owners (if (u/uid)
                     (db-get-active-all-uids)
                     (db-get-active-public-uids)))
         (records (mapcar (lambda (x)
                            (let ((tval (db-latest-from-user x))
                                  (the-email (db-email-from-uid x)))
                              (if tval
                                  (progn
                                    (add-element "email" the-email tval)
                                    tval)
                                  nil)))
                          owners)))

    (w/json
     (jsown:to-json
      (mapcar #'(lambda (x)
                   (let* ((po (empty-object))
                          (newf
                           (mapcar (lambda (kk)
                                     (setf (jsown:val po kk)
                                           (format nil "~a"
                                                   (get-element kk x))))
                                   '("uid" "email" "longitude" "latitude"))))
                    (setf (jsown:val newf "timestamp")
                          (universal-to-unix-time
                           (get-element "timestamp" x)))
                    po))
              records)))))

(defun ajax/beacon ()
  (w/logged-in
   (let ((longitude (parameter "position[coords][longitude]"))
         (latitude (parameter "position[coords][latitude]"))
         (my-uid (session-value "uid")))
     (with-pg (insert-dao
               (make-instance 'beacon
                              :uid my-uid
                              :latitude latitude
                              :longitude longitude)))
     (w/json "{result:'true'}"))))

(defun uid-is-public (uid)
  (let ((user-doc (car (with-pg
                         (query
                          (:select '* :from 'customer
                                   :where (:= 'uid uid)))))))
    (if user-doc
        (customer-public user-doc)
        nil)))

(defun db-get-active-public-uids ()
  (remove-if-not #'uid-is-public
                 (db-get-active-uids)))

(defun db-get-active-all-uids ()
  (@-m (get-element "values"
                    (car (docs (iter
                                (db.distinct "beacon" "owner")))))))

(defun db-latest-from-user (uid)
  (@-m
   (car (docs (iter (db.sort "beacon" ($ "owner" uid)
                             :limit 1
                             :asc nil
                             :field "timestamp"))))))

(defun db-email-from-uid (uid)
  (@-m
   (get-element "email"
                (car (docs (iter (db.find "users" ($ "uid" uid))))))))

(defun universal-to-unix-time (ut)
  (let ((epoch-difference (encode-universal-time 0 0 0 1 1 1970 0)))
    (- ut epoch-difference)))

(defun handler/site-js ()
  (no-cache)
  (setf (hunchentoot:content-type*) "text/javascript")
  (ps
    (defvar liaison (create
                     fx (lambda ()
                          (fsubmit "honk"))
                     showprefs (lambda ()
                                 ((@ ($ (@ "#prefs")) load) "/preferences")
                                 ((@ ($ (@ "#prefs")) modal) (create backdrop t
                                                                     keyboard t
                                                                     show t)))
                     recenter (lambda ()
                                ((@ navigator geolocation get-Current-Position)
                                 (lambda (pos)
                                   ((@ goog_map pan-To) (new ((@ google maps -Lat-Lng) (@ pos coords latitude) (@ pos coords longitude)))))))
                     init (lambda ()
                            (if navigator.geolocation
                                ((@ navigator geolocation get-Current-Position)
                                 (@ liaison isuccess)
                                 (@ liaison ierror))))
                     ifailure (lambda ()
                                nil)
                     isuccess (lambda (pos)
                                ((@ ($ "#status") toggle))
                                (setf goog_pos  (new ((@ google maps -Lat-Lng)
                                                      (@ pos coords latitude)
                                                      (@ pos coords longitude))))
                                (setf goog_map (new ((@ google maps -Map)
                                        ;((@ document get-Element-By-Id) "canvas")
                                                     (@ ((@ $) "#canvas") 0)
                                                     (create center goog_pos
                                                             zoom 15
                                                             map-Type-Id (@ google maps -Map-Type-Id -R-O-A-D-M-A-P)))))
                                true)
                     make_marker (lambda (ujs)
                                   (var d (@ (new ( -Date 0))))
                                   ((@ d set-U-T-C-Seconds) (@ ujs timestamp))
                                   (var mk (new ((@ google maps -Marker)
                                                 (create position (new ((@ google maps -Lat-Lng) (@ ujs latitude) (@ ujs longitude)))
                                                         shape (create coord (array 1 1 1 20 18 20 18 1)
                                                                       type "poly")
                                                         map goog_map
                                                         animation (@ google maps -Animation -D-R-O-P)
                                                         title (@ ujs uid)))))
                                   (var iw (new ((@ google maps -Info-Window)
                                                 (create content (concatenate 'string
                                                                              (@ ujs email)
                                                                              "<br/>"
                                                                              d)))))
                                   ((@ google maps event add-Listener) mk "click"
                                    (lambda ()
                                      ((@ goog_map pan-To) (new ((@ google maps -Lat-Lng) (@ ujs latitude) (@ ujs longitude))))
                                      ((@ goog_map set-Zoom) 17)
                                      ((@ iw.open) goog_map mk)))
                                   ((@ goog_markers push) mk)
                                   true)
                     clearmap (lambda ()
                                ((@ $ each) goog_markers (lambda (idx val)
                                                           ((@ val set-Map) nil)
                                                           true)))
                     loader (lambda (tval)
                              (let ((timeval (or tval
                                                 "yesterday")))
                                ((@ $ each) goog_markers (lambda (idx val)
                                                           ((@ val set-Map) nil)
                                                           true))
                                
                                ((@ $ get-j-s-o-n) "/gather" ;(concatenate 'string "/gather/" timeval)
                                 (lambda (dat)
                                   ((@ $ each)
                                    dat
                                    (lambda (k v)
                                      ((@ liaison make_marker) v)))))
                                ((@ ($ "#tstatus") html) "Updated!")
                                ((@ ($ "#tstatus") fade-Out) 12000)))
      
                     beacon (lambda ()
                              (and (@ navigator geolocation)
                                   ((@ navigator geolocation get-Current-Position)
                                    (lambda (pos)
                                      ((@ $ ajax) (create
                                                   type "POST"
                                                   url "/beacon"
                                                   data (create position pos)))))))
                     prefload (lambda (prefo)
                                ((@ $ ajax) (create url (concatenate 'string "/settings/get/" prefo)
                                                    data-Type "json"
                                                    success (lambda (x)
                                                              (if (string= (@ x result) "true")
                                                                  ((@ ($ (concatenate 'string "#" prefo)) attr) "checked" "true")
                                                                  ((@ ($ (concatenate 'string "#" prefo)) attr) "checked" false))))))
                                        ; ((@ ($ (@ (concatenate 'string "#" prefo))) val) "checked"))
                     prefssetup (lambda ()
                                  ((@ $ each) '(search public) (lambda (x y)
                                                                 ((@ liaison prefload) y))))

                     mkimage (lambda ()
                               (new ((@ google maps -Marker-Image) "/girls_marker.png"
                                     (new ((@ google maps -Size) 20 32))
                                     (new ((@ google maps -Point) 0 0))
                                     (new ((@ google maps -Point) 0 32)))))))
    ((@ ($ document) ready) (lambda ()
                              ((@ liaison init))
                              (set-Timeout (lambda ()
                                             ((@ liaison loader))) 2000)
                              (set-Timeout (@ liaison beacon) 30000)
                              (set-Timeout (@ liaison loader) 60000)))))

(defun page-content (url)
  (multiple-value-bind (content status headers uri stream must-close phrase)
      (drakma:http-request url)
    (declare (ignore headers))
    (when must-close
      (close stream))
    (unless (= status 200)
           (error "unexpected status ~A {~A} on ~A" status phrase uri))
    content))

(defun json-rsp (lst)
  (let* ((ox (empty-object))
         (to-send (mapcar (lambda (x)
                            (setf (jsown:val ox (car x)) (second x)))
                          lst)))
    (w/json (jsown:to-json to-send))))

(defun handler/settings ()
;; <<<<<<< HEAD
;;   (labels ((toggle-setting (name)
;;              (let* ((the-uid (u/uid))
;;                     (the-user-doc (car (@-q "users" ($ "uid" the-uid))))
;;                     (the-value (parameter "value")))
;;                (if the-user-doc
;;                    (progn
;;                      (if the-value
;;                          (add-element name "1" the-user-doc)
;;                          (rm-element name the-user-doc))
;;                      (@-m (db.save "users" the-user-doc))
;;                      (json-rsp '(("result" "success"))))
;;                    (json-rsp '(("result" "no user doc??")))))))

;;     (w/logged-in
;;      (let* ((the-setting (parameter "name")))
;;        (and the-setting
;;             (toggle-setting the-setting))))))
;; =======
  (w/session
   (let ((user-uid (session-value :uid))
         (the-setting (parameter "name"))
         (the-value (parameter "value")))
     (when user-uid
       (with-mongo-connection (:db "liaison")
         (let ((user-doc (car (docs (db.find "users" ($ "uid" user-uid))))))
           (when (and the-setting user-doc)
             (progn
               (if the-value
                   (add-element the-setting "1" user-doc)
                   (rm-element the-setting user-doc))
               (db.update "users" ($ "uid" user-uid) user-doc)
               (json-rsp '(("result" "success")))))))))))

(defun one-level-json (n v)
  (setf (jsown:val (jsown:empty-object) n) v))

(defun handler/preference ()
;; <<<<<<< HEAD
;;   (let ((pref-option (cl-ppcre:regex-replace-all "/settings/get/" (request-uri*) "")))
;;     (if (has-preference pref-option)
;;         (w/json (jsown:to-json (one-level-json "result" "true")))
;;         (w/json (jsown:to-json (one-level-json "result" "false"))))))

;; =======
  (let* ((my-uid (w/session (session-value :uid)))
         (pref-option (cl-ppcre:regex-replace-all "/settings/get/" (request-uri*) "")))
    (when my-uid
      (let ((the-user-doc (with-mongo-connection (:db "liaison")
                            (car (docs (db.find "users" ($ "uid" my-uid) :limit 1))))))
        (if (get-element pref-option the-user-doc)
            (w/json (jsown:to-json (one-level-json "result" "true")))
            (w/json (jsown:to-json (one-level-json "result" "no"))))))))
