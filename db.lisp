
(in-package :liaison)

(defmacro with-pg (&body body)
  `(postmodern:with-connection
       (list "liaison" "cmoore" "fl33j0b" "127.0.0.1" :pooled-p t)
     ,@body))

(defclass customer ()
  ((uid :accessor customer-uid
        :initform (unique-id)
        :col-type string)
   (email :accessor customer-email
          :initarg :email
          :col-type string)
   (password :accessor customer-password
             :initarg :password
             :col-type string))
  (:metaclass dao-class)
  (:keys uid))

(defclass beacon ()
  ((uid :accessor beacon-uid
        :initform (unique-id)
        :col-type string)
   (owner :accessor beacon-owner
             :initarg :owner
             :col-type string)
   (timestamp :accessor beacon-timestamp
              :initform (get-universal-time))
   (latitude :accessor beacon-latitude
             :initarg :latitude
             :col-type string)
   (longitude :accessor beacon-longitude
              :initarg :longitude
              :col-type string))
  (:metaclass dao-class)
  (:keys uid))


(defun reset-tables ()
  (with-pg
    (execute "drop table if exists beacon")
    (execute (dao-table-definition 'beacon)))
  (with-pg
    (execute "drop table if exists customer")
    (execute (dao-table-definition 'customer))))
