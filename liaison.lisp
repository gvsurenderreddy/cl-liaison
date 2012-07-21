; -*- mode: Lisp eval: (hs-hide-all) -*-

(in-package #:liaison)

(setf lparallel:*kernel* (make-kernel 16))

;(defparameter *io-acceptor* nil)
(defparameter *site-acceptor* nil)
(defparameter *dispatch-table* nil)

;; (define-socket.io-handler (lambda (message)
;;                             (declare (ignore message))))

;; (socket.io-on "connection" (session)
;;   (declare (ignore session))
;;   (socket.io-emit "news" '((:hello . "world")))
;;   (socket.io-on "my other event" (data)
;;     (log-message :debug data)))

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
     (setf (session-max-time hunchentoot:*session*) (* 60 60 24 265))
     ,@body))
(defmacro w/logged-in (&rest body)
  `(let* ((uid (session-value :uid))
          (user-doc (car (@-q "users" ($ "uid" uid)))))
     (if user-doc
         ,@body
         (hunchentoot:redirect "/login"))))

(defmacro @-m (&rest body)
  `(with-mongo-connection (:db "liaison")
     ,@body))
(defmacro @-q (collection query &optional args)
  `(@-m (docs (iter (db.find ,collection ,query ,@args)))))
(defmacro @-> (name)
  `(hunchentoot:parameter ,name))
(defmacro re/kill (pattern target)
  `(cl-ppcre:regex-replace-all ,pattern ,target ""))
(defmacro w/ajax (&rest body)
  `(cl-who:with-html-output-to-string
       (*standard-output* nil :prologue nil :indent t)
     ,@body))

(defmacro w/page (title &rest body)
  `(cl-who:with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
       (:title ,title)
       (:link :type "text/css" :rel "stylesheet" :href "/bs/css/bootstrap.css")
       (:link :type "text/css" :rel "stylesheet" :href "/bs/css/bootstrap-responsive.css")
       (:link :type "text/css" :rel "stylesheet" :href "http://fonts.googleapis.com/css?family=Anonymous+Pro|Cantarell|Ubuntu|Ubuntu+Mono")
       (:link :type "text/css" :rel "stylesheet" :href "/css")
       (:link :rel "shortcut icon" :href "/favicon.ico")
       (:link :rel "apple-touch-icon" :href "/bs/images/apple-touch-icon.png")
       (:link :rel "apple-touch-icon" :sizes "72x72" :href "/bs/images/apple-touch-icon-72x72.png")
       (:link :rel "apple-touch-icon" :sizes "114x114" :href "/bs/images/apple-touch-icon-114x114.png")
       (:script :type "text/javascript" :src "/jquery-min.js"))
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
        ,@body
        (dialog-msg)
        (htm
         (:script :src "/bs/js/bootstrap.js"))))))

(defun page/main ()
  (let ((my-uid (u/uid)))
    (no-cache)
    (w/page "Geo"
         (:script :src "/js")
         (:script :type "text/javascript"
                  :src "http://maps.google.com/maps/api/js?sensor=false&key=AIzaSyDsOVRkRfKm3kBVrUaih3xRPYp6dRe8iZ4")
         (:script :type "text/javascript"
                  (str (ps (var goog_markers ([]))
                           (var goog_map nil))))
         (:script :type "text/javascript"
                  (str (ps ((@ ($ document) ready)
                            (lambda ()
                              ((@ ($ "#canvas") css) "margin-top" "45px"))))))

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
                                                                         (:input :id "profpublic"
                                                                                 :type "checkbox"
                                                                                 :name "public"
                                                                                 :onclick (ps-inline (settings-toggle "#profpublic" "public")))
                                                                         (:p :class "help-block"
                                                                             "If checked, your profile is publicly viewable to people who ARE NOT yet logged in.")))
                                                      (%-cgroup :name "Searchable"
                                                                :inner (:span
                                                                        (:input :id "profsearch"
                                                                                :type "checkbox"
                    
                                                            :name "searchable"
                                                                                :onclick (ps-inline (settings-toggle "#profsearch" "search")))
                                                                        (:p :class "help-block"
                                                                            "If checked, your profile is searchable by other users.  Note that users who are not logged in CANNOT search at all.")))
                                                      ;; (%-cgroup :name "Profile Picture"
                                                      ;;           :inner (:span 
                                                      ;;                   (:input :type "file"
                                                      ;;                           :name "ppic"
                                                      ;;                           :class "span2")))
                                                      (%-cgroup :name "Gender"
                                                                :inner (:span (:select :id "gselect" :name "gender"
                                                                                       (:option :value "noselect" "  ")
                                                                                       (:option :value "female" "Female")
                                                                                       (:option :value "male" "Male")
                                                                                       (:option :value "male tg" "Male Transgendered")
                                                                                       (:option :value "female tg" "Female Transgendered")
                                                                                       (:option :value "very male" "Alpha Male")
                                                                                       (:option :value "femmy" "Male, but a little female.")
                                                                                       (:option :value "very female" "Alpha Female")
                                                                                       (:option :value "butchy" "Female, somewhat masculine."))
                                                                              (:p :class "help-block" "Not required, but definitely recommended.")))
                                                      (%-cgroup :name "Name"
                                                                :inner (:input :id "pseudonym" :type "text" :name "name" :placeholder "or pseudonym"))
                                                      (%-cgroup :name "Age"
                                                                :inner (:select :id "agef"
                                                                                :name "age"
                                                                                (:option :value "18-20" "18-20")
                                                                                (:option :value "20-25" "20-25")
                                                                                (:option :value "25-30" "25-30")
                                                                                (:option :value "30-35" "30-35")
                                                                                (:option :value "35-40" "35-40")
                                                                                (:option :value "40-45" "40-45")
                                                                                (:option :value "45-50" "45-50")
                                                                                (:option :value "50-55" "50-55")
                                                                                (:option :value "55-60" "55-60")
                                                                                (:option :value "60-65" "60-65")
                                                                                (:option :value "65-70" "65-70")
                                                                                (:option :value "70+" "70+")))

                                                      (%-cgroup :name "Bio"
                                                                :inner (:span (:textarea :name "bio" "The Bio.")))))
                              (%-agroup :name "Preferences"
                                        :dataparent "#cpanel"
                                        :inner (:form :id "prefsform" :class "form-horizontal" :method "POST" :action "/preferences"
                                                      (:input :id "p-uid" :type "hidden" :name "uid" :value (str my-uid))
                                                      (%-cgroup :name "Gender"
                                                                :inner (:span (:select :id "gselect" :name "gender"
                                                                                       (:option :value "noselect" "  ")
                                                                                       (:option :value "female" "Female")
                                                                                       (:option :value "male" "Male")
                                                                                       (:option :value "male tg" "Male Transgendered")
                                                                                       (:option :value "female tg" "Female Transgendered")
                                                                                       (:option :value "very male" "Alpha Male")
                                                                                       (:option :value "femmy" "Male, but a little female.")
                                                                                       (:option :value "very female" "Alpha Female")
                                                                                       (:option :value "butchy" "Female, somewhat masculine."))
                                                                              (:p :class "help-block" "Not required, but definitely recommended.")))
                                                      (%-cgroup :name "Name"
                                                                :inner (:input :id "pseudonym" :type "text" :name "name" :placeholder "or pseudonym"))
                                                      (%-cgroup :name "Age"
                                                                :inner (:select :id "agef"
                                                                                :name "age"
                                                                                (:option :value "18-20" "18-20")
                                                                                (:option :value "20-25" "20-25")
                                                                                (:option :value "25-30" "25-30")
                                                                                (:option :value "30-35" "30-35")
                                                                                (:option :value "35-40" "35-40")
                                                                                (:option :value "40-45" "40-45")
                                                                                (:option :value "45-50" "45-50")
                                                                                (:option :value "50-55" "50-55")
                                                                                (:option :value "55-60" "55-60")
                                                                                (:option :value "60-65" "60-65")
                                                                                (:option :value "65-70" "65-70")
                                                                                (:option :value "70+" "70+")))
                                                      (:button :class "btn btn-primary" "Save")))
                              (%-agroup :name "Developer Console"
                                        :dataparent "#cpanel"
                                        :inner (:form :id "devform" :class "form-horizontal"
                                                      (%-cgroup
                                                       :name "Development Mode"
                                                       :inner (:span (:input :id "devtog"
                                                                             :type "checkbox"
                                                                             :name "devmode"
                                                                             :onclick (ps-inline (settings-toggle "#devtog" "developer")))
                                                                     (:p :class "help-block" "If checked, disables the map, among other things.")))))))))))

(defun %-random-element-id ()
  "Build a random string to use as an HTML element id."
  (let ((tid (string (gensym))))
    (list tid (concatenate 'string "#" tid))))
(defmacro %-agroup (&key name dataparent inner)
    (let ((the-inner-div (%-random-element-id)))
      `(htm (:div :class "accordion-group"
                  (:div :class "accordion-heading"
                        (:a :class "accordion-toggle"
                            :data-toggle "collapse"
                            :data-parent ,dataparent
                            :href ,(second the-inner-div) ,name))
                  (:div :id ,(car the-inner-div)
                        :class "accordion-body collapse"
                        (:div :class "accordion-inner"
                              ,inner))))))
(defmacro %-cgroup (&key name inner targetid)
  (let ((target-id (if targetid
                       (concatenate 'string "#" targetid)
                       (second (%-random-element-id))))
        (the-inner (or inner
                       (htm (:div "Nothing sent for inner html!")))))
                       
    `(htm (:div :class "control-group"
          (:label :class "control-label" :for ,target-id ,name)
          (:div :class "controls"
            ,the-inner)))))

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
          (htm
           (:div :class "alert alert-danger"
                 (str msg))))))))
(defun check/email-exists (email)
  (@-q "users" ($ "email" email)))

(defun $-replace (pat buf)
  (cl-ppcre:regex-replace pat (format nil "~a" buf) ""))
(defun hash-password (pas)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256
    (ironclad:ascii-string-to-byte-array pas))))

(defun unique-id ()
  (format nil "~a"
    (uuid:make-v4-uuid)))

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
                    (the-doc (car (@-q "users" ($ ($ "email" the-email)
                                                  ($ "password"
                                                     (hash-password the-pass)))))))
               (if the-doc
                   (w/session
                    (setf (session-value :uid )
                          (get-element "uid" the-doc))
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
                                    (@-m
                                      (add-element "email" new-email td)
                                      (add-element "uid" uu td)
                                      (add-element "password" (hash-password new-pass) td)
                                      (add-element "realname" "Real Name" td)
                                      (db.save "users" td)
                                      (dialog-set "Registration successful!")
                                      (w/session
                                       (setf (session-value "uid") uu)
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
(defun handler/site-css ()
  (setf (hunchentoot:content-type*) "text/css")
  (css-lite:css
    (("*") (:font-family "Open Sans,Ubuntu,arial"))
    (("html,body") (:height "100%"))
    ((".container,.row,#canvas,.span12") (:height "inherit"))))

(defun ajax/gather ()
  (no-cache)
  (let* ((owners (db-get-active-uids))
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
   (labels ((normalize-name (f)
              (re/kill "position"
                 (re/kill "coords"
                     (re/kill "\\]+|\\[+" f)))))
     (let* ((new-doc (make-document))
            (my-owner (u/uid))
            (my-uid (unique-id))
            (all-keys (hunchentoot:post-parameters*)))
       (if (< 0 (length all-keys))
           (@-m
             (loop for (a . b) in all-keys
                do (progn
                     (or (string= b "null")
                         (add-element (normalize-name a) b new-doc))))
             (add-element "owner" my-owner new-doc)
             (add-element "uid" my-uid new-doc)
             (add-element "timestamp" (get-universal-time) new-doc)
             (db.save "beacon" new-doc)
             (w/json "{result:'true'}"))
           (w/json "{result:'failed'}"))))))

(defun db-get-active-uids ()
  (@-m (get-element "values"
          (car (docs (iter (db.distinct "beacon" "owner")))))))
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

(defpsmacro fsubmit (name)
  `(((@ $ each) ((@ ($ ,name) find) "input")) (lambda (i x)
                                                (let ((nam (@ x name))
                                                      (val (@ x value)))
                                                  '(nam val)))))
(defpsmacro settings-toggle (htmlelement remotename)
  "Used mainly for checkboxes, click the box, set the option in the database."
  `((@ $ ajax) (create
                 type "POST"
                 url "/settings/toggle"
                 data (create name ,remotename
                              value ((@ ((@ $ ) ,htmlelement) attr) "checked")))))

(defun handler/site-js ()
  (no-cache)
  (setf (hunchentoot:content-type*) "text/javascript")
  (ps
    (var
     liaison
     (create
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
                                  (create
                                   position (new ((@ google maps -Lat-Lng) (@ ujs latitude) (@ ujs longitude)))
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
                                            
                 ((@ $ get-J-S-O-N) "/gather" ;(concatenate 'string "/gather/" timeval)
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
                 (ps ((@ $ ajax) (create
                                  url (concatenate 'string "/settings/get/" prefo)
                                  data-Type "json"
                                  success (lambda (x)
                                            (and (string= (@ x result) "true")
                                                 ((@ (concatenate 'string "#" prefo) val) "checked"))))))
                                                 ((@ ($ (@ (concatenate 'string "#" prefo))) val) "checked"))
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
  (labels ((toggle-setting (name)
             (let* ((the-uid (u/uid))
                    (the-user-doc (car (@-q "users" ($ "uid" the-uid))))
                    (the-value (parameter "value")))
               (if the-user-doc
                   (progn
                     (if the-value
                         (add-element name "1" the-user-doc)
                         (rm-element name the-user-doc))
                     (@-m (db.save "users" the-user-doc))
                     (json-rsp '(("result" "success"))))
                   (json-rsp '(("result" "no user doc??")))))))

    (w/logged-in
     (let* ((the-setting (parameter "name")))
       (and the-setting
            (toggle-setting the-setting))))))

(defun one-level-json (n v)
  (setf (jsown:val (jsown:empty-object) n) v))

(defun has-preference (n)
  (let ((the-user-doc (@-q "users" ($ "uid" (u/uid)))))
    (get-element n the-user-doc)))

(defun handler/preference ()
  (let ((pref-option (cl-ppcre:regex-replace-all "/settings/get/" (request-uri*) "")))
    (if (has-preference pref-option)
        (w/json (jsown:to-json (one-level-json "result" "true")))
        (w/json (jsown:to-json (one-level-json "result" "false"))))))


