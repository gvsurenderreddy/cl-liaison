

(in-package :liaison)

(defmacro ext-random-element-id ()
  (let ((tid (string (gensym))))
    `(list ,tid ,(concatenate 'string "#" tid))))

(defmacro %-agroup (&key name dataparent inner)
    (let ((the-inner-div (ext-random-element-id)))
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
                       (second (ext-random-element-id))))
        (the-inner (or inner
                       (htm (:div "Nothing sent for inner html!")))))
                       
    `(htm (:div :class "control-group"
          (:label :class "control-label" :for ,target-id ,name)
          (:div :class "controls"
            ,the-inner)))))

(defmacro w/session (&rest body)
  `(progn
     (hunchentoot:start-session)
     (setf (session-max-time hunchentoot:*session*) (* 60 60 24 265))
     ,@body))

(defmacro w/logged-in (&rest body)
  `(let ((uid (session-value :uid)))
     (if uid
         (let ((the-customer (car (with-pg (query
                                            (:select '* :from 'customer
                                                     :where (:= 'uid uid)))))))
           (if the-customer
               ,@body
               (hunchentoot:redirect "/login")))
         (redirect "/login"))))





(defmacro re/kill (pattern target)
  `(cl-ppcre:regex-replace-all ,pattern ,target ""))

(defmacro w/ajax (&rest body)
  `(cl-who:with-html-output-to-string
       (*standard-output* nil :prologue nil :indent t)
     ,@body))

(defmacro with-plain-page (&rest body)
  `(cl-who:with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
       (:link :type "text/css" :rel "stylesheet" :href "/bs/css/bootstrap.css")
       (:link :type "text/css" :rel "stylesheet" :href "/bs/css/login.css")
       (:link :type "text/css" :rel "stylesheet" :href "/bs/css/bootstrap-responsive.css")
       (:link :type "text/css"
              :rel "stylesheet"
              :href "https://fonts.googleapis.com/css?family=Anonymous+Pro|Cantarell|Ubuntu|Ubuntu+Mono")
       (:link :type "text/css" :rel "stylesheet" :href "/css")
       (:link :rel "shortcut icon" :href "/favicon.ico")
       (:link :rel "apple-touch-icon" :href "/bs/images/apple-touch-icon.png")
       (:link :rel "apple-touch-icon" :sizes "72x72" :href "/bs/images/apple-touch-icon-72x72.png")
       (:link :rel "apple-touch-icon" :sizes "114x114" :href "/bs/images/apple-touch-icon-114x114.png")
       (:script :type "text/javascript" :src "/jquery-min.js"))
      ,@body
      (dialog-msg))))
