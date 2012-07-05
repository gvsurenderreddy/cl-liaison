
(defpackage :lutils
  (:use :cl
        :alexandria
        :cl-mongo
        :jsown
        :cl-moore
        :parenscript
        :cl-who)
  (:shadowing-import-from :lparallel :chain)
  (:shadowing-import-from :parenscript :switch))

(defun e@-symbol? (s)
  (let ((sname (symbol-name s)))
    (and (symbolp s)
       (> (length sname) 2)
       (string= sname "E@" :start1 0 :end1 2))))

(defmacro defmacro@ (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'e@-symbol?
                              (alexandria:flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq (symbol-name s) 2))))
              syms)
         ,@body))))

(defmacro %-> (doc name val)
  `(add-element ,name ,val ,doc))

(defmacro <-% (doc name)
  `(get-element ,name ,doc))


(defparameter tt (make-document))
(macroexpand (%-> tt "one" "two")
