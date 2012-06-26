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
       (hunchentoot:create-prefix-dispatcher "/logout" 'handler/logout)
       (hunchentoot:create-prefix-dispatcher "/register" 'handler/register)

       (hunchentoot:create-prefix-dispatcher "/beacon" 'ajax/beacon)
       (hunchentoot:create-prefix-dispatcher "/gather" 'ajax/load-map)

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
(defmacro @-q (collection query)
  `(docs (iter (db.find ,collection ,query :limit 0))))
(defmacro @-> (name)
  `(hunchentoot:parameter ,name))
(defmacro re/kill (pattern target)
  `(cl-ppcre:regex-replace-all ,pattern ,target ""))
(defmacro w/ajax (&rest body)
  `(cl-who:with-html-output-to-string
       (*standard-output* nil :prologue nil :indent nil)
     ,@body))
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
       (:link :type "text/css" :rel "stylesheet" :href "/liaison.css")
       (:link :type "text/css" :rel "stylesheet" :href "/css")
       (:link :rel "shortcut icon" :href "/favicon.ico")
       (:link :rel "apple-touch-icon" :href "/bs/images/apple-touch-icon.png")
       (:link :rel "apple-touch-icon" :sizes "72x72" :href "/bs/images/apple-touch-icon-72x72.png")
       (:link :rel "apple-touch-icon" :sizes "114x114" :href "/bs/images/apple-touch-icon-114x114.png")
       (:script :type "text/javascript" :src "/jquery-min.js")
       (:script (ps (defvar goog_map nil)
                    (defvar goog_markers nil))))
      (:body :style "padding-top: 10px;"
             ,@body
             (dialog-msg)
             (htm
              (:div :class "navbar navbar-fixed-bottom"
               (:div :class "navbar-inner"
                 (:div :class "container"
                   (if (w/session (session-value :uid))
                       (htm (:ul :class "nav"
                              (:a :href "/" :class "brand" "Liaison")
                              (:li :class "dropdown"
                                (:a :href "#"
                                    :class "dropdown-toggle"
                                    :data-toggle "dropdown"
                                    (:i :class "icon-upload icon-white"))
                                    
                                (:ul :class "dropdown-menu"
                                     (:li :class "nav-header" "Options")
                                     (:li :class "nav-divider")
                                     (:li (:a :data-toggle "modal"
                                              :href "#pmodal"
                                              "Preferences"))
                                     
                                     (:li :class "nav-header" "Map Options")
                                     (:li :class "nav-divider")
                                     (:li (:a :href "#"
                                              :onclick (ps ((@ liaison beacon))
                                                           false)
                                              "Ping"))
                                     (:li (:a :href "#"
                                              :onclick (ps ((@ liaison clearmap))
                                                           false)
                                              "Clear The Map"))
                                     (:li :class "nav-header" "People")
                                     (:li :class "nav-divider")
                                     (:li (:a :href "#"
                                              :onclick (ps ((@ liaison loader))
                                                           false)
                                             "Active within 10 minutes"))
                                     (:li (:a :href "#"
                                              :onclick (ps ((@ liaison loader))
                                                           false)
                                             "Active within 30 minutes"))
                                     (:li (:a :href "#"
                                              :onclick (ps ((@ liaison loader))
                                                           false)
                                             "Active within 2 hours"))
                                     (:li (:a :href "#"
                                              :onclick (ps ((@ liaison loader))
                                                           false)
                                             "Active within 1 day")))))
                            (:ul :class "nav pull-right"
                                 (:li (:a :href "/logout" "Logout"))))
                       (htm
                         (:li (:a :href "/" (:i :class "icon-home icon-white")))
                         (:li (:a :href "/login" "Login"))))))))
             (htm
              (:script :src "/bs/js/bootstrap.js")
              (:script :src "/js")
              (:script :type "text/javascript" :src "http://maps.google.com/maps/api/js?sensor=false&key=AIzaSyDsOVRkRfKm3kBVrUaih3xRPYp6dRe8iZ4"))))))

(defun u/uid ()
  (w/session
   (session-value :uid)))
(defun w/json (msg)
  (setf (hunchentoot:content-type*) "application/json")
  msg)
(defun unique-id ()
  (format nil "~a"
    (uuid:make-v4-uuid)))
(defun dialog-set (m)
  (w/session (setf (session-value :status) m)))
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
(defun page/main ()
  (let ((my-uid (u/uid)))
    (no-cache)
    (w/page "Geo"
            (:script :type "text/javascript"
                     (str (ps (var goog_markers ([]))
                              (var goog_map nil))))
            (:div :class "container"
                  (:div :class "row"
                        (:div :id "status" "Loading...")
                        (:div :class "well" :id "canvas"))
                  (:div :id "pmodal" :class "modal hide fade"
                        (:div :class "modal-header"
                              (:button :type "button" :class "close" :data-dismiss "modal" "x")
                              (:h4 "Preferences"))
                        (:div :class "modal-body"
                              (:div :class "accordion" :id "cpanel"
                                    (:div :class "accordion-group"
                                          (:div :class "accordion-heading"
                                                (:a :class "accordion-toggle"
                                                    :data-toggle "collapse"
                                                    :data-parent "#cpanel"
                                                    :href "#friends" "Friends"))
                                          (:div :id "friends"
                                                :class "accordion-body collapse in"
                                                (:div :class "accordion-inner"
                                                      "This is a list of your friends.")))

                                    (:div :class "accordion-group"
                                          (:div :class "accordion-heading"
                                                (:a :class "accordion-toggle"
                                                    :data-toggle "collapse"
                                                    :data-parent "#cpanel"
                                                    :href "#filters" "Filters"))
                                          (:div :id "filters"
                                                :class "accordion-body collapse"
                                                (:div :class "accordion-inner"
                                                      "This is a list of your filters.")))


                                    (%-agroup :name "Preferences"
                                              :dataparent "#cpanel"
                                              :inner (:form :id "prefsform" :class "form-horizontal" :method "POST" :action "/preferences"
                                                            (:input :type "hidden" :name "uid" :value (str my-uid))
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


                                                            (%-cgroup :name "Name" :inner (:input :id "pseudonym" :type "text" :name "name" :placeholder "or pseudonym"))
                                                            (%-cgroup :name "Age" :inner (:select :id "agef" :name "age"
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
                                    (:div :class "accordion-group"
                                          (:div :class "accordion-heading"
                                                (:a :class "accordion-toggle"
                                                    :data-toggle "collapse"
                                                    :data-parent "#cpanel"
                                                    :href "#devel" "Developer Console"))
                                          (:div :id "devel"
                                                :class "accordion-body collapse"
                                                (:div :class "accordion-inner"
                                                      "Developer Controls!"))))))))))


(defun %-r-eid ()
  "Build a random string to use as an HTML element id."
  (let* ((tid (string (gensym)))
        (dp (concatenate 'string "#" tid)))
    (list tid dp)))
(defmacro %-agroup (&key name dataparent inner)
  (let ((the-inner-div (%-r-eid)))
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
  (let ((target-id (or targetid
                       (second (%-r-eid))))
        (the-inner (or inner
                       (htm (:div "Nothing sent for inner html!")))))
                       
    `(htm (:div :class "control-group"
          (:label :class "control-label" :for ,target-id ,name)
          (:div :class "controls"
            ,the-inner)))))
            

(defun hash-password (pas)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256
    (ironclad:ascii-string-to-byte-array pas))))
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
                                    (progn
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
    ((:body) (:margin-bottom "0px"))
    ((".prefs") (:margin-top "20px"
                 :padding-left "20px"
                 :padding-right "50px"))))
(defun handler/site-js ()
  (no-cache)
  (setf (hunchentoot:content-type*) "text/javascript")
  (ps
    (var
     liaison
     (create

      showprefs (lambda ()
                  ((@ ($ (@ "#prefs")) load) "/preferences")
                  ((@ ($ (@ "#prefs")) modal) (create backdrop t
                                                      keyboard t
                                                      show t)))
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
                                         ((@ document get-Element-By-Id) "canvas")
                                          (create center goog_pos
                                                 zoom 15
                                                 map-Type-Id (@ google maps -Map-Type-Id -R-O-A-D-M-A-P)))))
                     true)



      make_marker (lambda (lat lon the_title)
                    (var mk (new ((@ google maps -Marker)
                                  (create
                                   position (new ((@ google maps -Lat-Lng) lat lon))
                                   shape (create coord (array 1 1 1 20 18 20 18 1)
                                                 type "poly")
                                   image ((@ liaison mkimage))
                                   ; shadow ((@ liaison mkshadow))
                                   map goog_map
                                   title the_title))))
                    ((@ goog_markers push) mk)
                    true)

      clearmap (lambda ()
                     ((@ $ each) goog_markers (lambda (idx val)
                                                ((@ val set-Map) nil)
                                                true)))
      loader (lambda ()
               ((@ $ each) goog_markers (lambda (idx val)
                                          ((@ val set-Map) nil)
                                          true))
                                            
               ((@ $ get-J-S-O-N) "/gather" (lambda (dat)
                                              ((@ $ each)
                                               dat
                                               (lambda (k v)
                                                 ((@ liaison make_marker)
                                                  (@ v latitude)
                                                  (@ v longitude)
                                                  (@ v uid)))))))


      beacon (lambda ()
               (and (@ navigator geolocation)
                    ((@ navigator geolocation get-Current-Position)
                     (lambda (pos)
                       ((@ $ ajax) (create
                                    type "POST"
                                    url "/beacon"
                                    data (create position pos)))
                       true)
                     (lambda () false))))
      mkimage (lambda ()
               (new ((@ google maps -Marker-Image) "/girls_marker.png"
                     (new ((@ google maps -Size) 20 32))
                     (new ((@ google maps -Point) 0 0))
                     (new ((@ google maps -Point) 0 32)))))))
      ;; mkshadow (lambda ()
      ;;            (new ((@ google maps -Marker-Image) "images/beachflag_shadow.png"
      ;;                  (new ((@ google maps -Size) 37 32))
      ;;                  (new ((@ google maps -Point) 0 0))
      ;;                  (new ((@ google maps -Point) 0 32)))))))
    
    ((@ ($ document) ready) (lambda ()
                              ((@ liaison init))
                              (set-Interval (@ liaison beacon) 30000)))))

(defun ajax/load-map ()
  (no-cache)
  (let* ((cts (chronicity:parse "four days ago"))
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
         (people (docs (iter (db.find "beacon" :all)))))
                                      ;; ($
                                      ;;  ($ "timestamp"
                                      ;;     ($ "$gte" mongo-timestamp)))
                                      ;; :limit 20)))))
    (w/json
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
(defun ajax/beacon ()
  (w/logged-in
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
             (w/json "{result:'true'}"))
           (w/json "{result:'failed'}"))))))






                                    ;; (:div :class "accordion-group"
                                    ;;       (:div :class "accordion-heading"
                                    ;;             (:a :class "accordion-toggle"
                                    ;;                 :data-toggle "collapse"
                                    ;;                 :data-parent "#cpanel"
                                    ;;                 :href "#prefs" "Preferences"))
                                    ;;       (:div :id "prefs"
                                    ;;             :class "accordion-body collapse"
                                    ;;             (:div :class "accordion-inner"
                                    ;;                   (:form :id "prefsform" :class "form-horizontal" :method "POST" :action "/preferences"
                                    ;;                          (:input :type "hidden" :name "uid" :value (str my-uid))
                                    ;;                          (%-cgroup :name "Gender" :inner (:div :class "control-group"
                                    ;;                                                                (:label :class "control-label" :for  "gselect" "Gender")
                                    ;;                                                                (:div :class "controls"
                                    ;;                                                                      (:select :id "gselect" :name "gender"
                                    ;;                                                                               (:option :value "noselect" "  ")
                                    ;;                                                                               (:option :value "female" "Female")
                                    ;;                                                                               (:option :value "male" "Male")
                                    ;;                                                                               (:option :value "male tg" "Male Transgendered")
                                    ;;                                                                               (:option :value "female tg" "Female Transgendered")
                                    ;;                                                                               (:option :value "very male" "Alpha Male")
                                    ;;                                                                               (:option :value "femmy" "Male, but a little female.")
                                    ;;                                                                               (:option :value "very female" "Alpha Female")
                                    ;;                                                                               (:option :value "butchy" "Female, somewhat masculine."))
                                    ;;                                                                      (:p :class "help-block"
                                    ;;                                                                          "Not required, but definitely recommended."))))
                                    ;;                          (%-cgroup :name "Name" :inner (:input :id "pseudonym" :type "text" :name "name" :placeholder "or pseudonym"))
                                    ;;                          (%-cgroup :name "Age" :inner (:select :id "agef" :name "age"
                                    ;;                                                                (:option :value "18-20" "18-20")
                                    ;;                                                                (:option :value "20-25" "20-25")
                                    ;;                                                                (:option :value "25-30" "25-30")
                                    ;;                                                                (:option :value "30-35" "30-35")
                                    ;;                                                                (:option :value "35-40" "35-40")
                                    ;;                                                                (:option :value "40-45" "40-45")
                                    ;;                                                                (:option :value "45-50" "45-50")
                                    ;;                                                                (:option :value "50-55" "50-55")
                                    ;;                                                                (:option :value "55-60" "55-60")
                                    ;;                                                                (:option :value "60-65" "60-65")
                                    ;;                                                                (:option :value "65-70" "65-70")
                                    ;;                                                                (:option :value "70+" "70+")))


