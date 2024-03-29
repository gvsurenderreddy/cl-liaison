
(in-package :cl-liaison)

(defun login-page ()
  (with-plain-page
      (:div :class "container"
            (:form :class "form-signin"
                   (:h2 :class "form-signin-heading" "Please sign in.")
                   (:input :type "text" :class "input-block-level" :placeholder "Email")
                   (:input :type "password" :class "input-block-level" :placeholder "Password")
                   (:label :class "checkbox"
                           (:input :type "checkbox" :value "remember-me") "Remember me")
                   (:button :class "btn btn-large btn-priary" :type "submit" "Sign In")))))
