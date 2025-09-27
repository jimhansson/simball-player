;;;; main.lisp
;;;; Simball Player API-tjänst med Clack och routing

(defpackage :simball-player
  (:use :cl :clack :clack.middleware :lack.request :lack.response)
  (:import-from :clack.middleware.router :define-routes))
(in-package :simball-player)

;; Ladda beroenden
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(clack clack-router jonathan)))

(defparameter *strategies* '("default" "strategy1"))

(defun valid-strategy-p (strategy)
  (member strategy *strategies* :test #'string=))

(defun json-response (data &optional (status 200))
  (let ((body (jonathan:to-json data)))
    (list status '(("Content-Type" . "application/json")) (list body))))

(define-routes app
  ((GET "/:strategy/Player" (strategy)
    (if (valid-strategy-p strategy)
        (json-response (format nil "Simball Player API - ~A" strategy))
        (json-response '("error" . "Invalid strategy") 404)))

   (GET "/:strategy/Player/setup" (strategy)
    (if (valid-strategy-p strategy)
        (json-response
         '((name . "SimballBot")
           (primaryColor . ((jearsey . "blue") (pants . "white") (socks . "blue")))
           (secondaryColor . ((jearsey . "red") (pants . "black") (socks . "red")))))
        (json-response '("error" . "Invalid strategy") 404)))

   (POST "/:strategy/Player/update" (strategy)
    (if (valid-strategy-p strategy)
        (let* ((req (lack.request:create-request *request*))
               (body (jonathan:parse (or (gethash "raw-body" req) "{}")))
               ;; Här kan du anropa olika strategier baserat på 'strategy'
               (instructions
                '((p1Instructions . ((moveToX . 0) (moveToY . 0) (moveVelocity . 0)))
                  (p2Instructions . ((moveToX . 0) (moveToY . 0) (moveVelocity . 0)))
                  (p3Instructions . ((moveToX . 0) (moveToY . 0) (moveVelocity . 0)))
                  (p4Instructions . ((moveToX . 0) (moveToY . 0) (moveVelocity . 0))))))
          (json-response instructions))
        (json-response '("error" . "Invalid strategy") 404)))))

(defun start-server (&key (port 5000))
  (clack:clackup app :port port))

;; För att starta servern: (simball-player:start-server)
