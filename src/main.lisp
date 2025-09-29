;;;; main.lisp
;;;; Simball Player API-tjänst med Clack

(defpackage :simball-player
  (:use :cl :clack :lack/request :lack.response :cl-ppcre)
  (:export :start-server))

(in-package :simball-player)

;; Färgkonstanter för lagkläder

(defparameter +primary-jersey-blue+   "1A237E") ; Mörkblå
(defparameter +primary-pants-white+   "FFFFFF") ; Vit
(defparameter +primary-socks-blue+    "1A237E") ; Mörkblå
(defparameter +secondary-jersey-red+  "C62828") ; Röd
(defparameter +secondary-pants-black+ "000000") ; Svart
(defparameter +secondary-socks-red+   "C62828") ; Röd


(defparameter *strategies* '("default" "strategy1"))

;; Lägg till CORS-header i alla svar
(defparameter *cors-headers*
  '("Access-Control-Allow-Origin" "*" 
    "Access-Control-Allow-Headers" "Content-Type, Authorization" 
    "Access-Control-Allow-Methods" "GET, POST, OPTIONS"))


(defparameter *history-size* 10)

;; Struktur: correlationId => ((requests . fifo) (responses . fifo))
(defparameter *correlation-history* (make-hash-table :test 'equal))

(defun fifo-push (fifo item maxlen)
  "Lägg till ITEM sist i FIFO-listan, trimma till MAXLEN. Returnerar ny lista."
  (let ((new (append fifo (list item))))
    (if (> (length new) maxlen)
        (subseq new (- (length new) maxlen))
        new)))

(defun save-history (correlation-id req-body resp-body)
  (let* ((entry (gethash correlation-id *correlation-history*)))
    (if entry
        (progn
          (setf (cdr (assoc 'requests entry)) (fifo-push (cdr (assoc 'requests entry)) req-body *history-size*))
          (setf (cdr (assoc 'responses entry)) (fifo-push (cdr (assoc 'responses entry)) resp-body *history-size*))
          (setf (gethash correlation-id *correlation-history*) entry))
        (setf (gethash correlation-id *correlation-history*)
              (list (cons 'requests (list req-body))
                    (cons 'responses (list resp-body)))))))

(defun valid-strategy-p (strategy)
  (member strategy *strategies* :test #'string=))


(defun json-response (data &optional (status 200))
  (let ((body (jonathan:to-json data)))
    (list status
          (append *cors-headers* '(:content-type  "application/json"))
          (list body))))

;; Enkel dispatch-funktion för Clack
(defun app (env)
  (let* ((req (lack/request:make-request env))
         (method (string-upcase (request-method req)))
         (path (request-path-info req))
         (strategy-match (multiple-value-list (ppcre:scan-to-strings "^/([^/]+)/" path)))
         (strategy (and (first strategy-match) (aref (second strategy-match) 0))))
         (progn
         (format t "Request before cond: ~A ~A ~A ~%" method path strategy)
    (cond
      ;; OPTIONS för CORS preflight
      ((string= method "OPTIONS")
       (list 200 *cors-headers* '("")))

      ;; GET /:strategy/Player
      ((and (string= method "GET")
            (valid-strategy-p strategy))
       (progn
        (format t "Strategy: ~A~%" strategy)
        (let ((strategy (second (ppcre:split "/" path))))
            (json-response (format nil "Simball Player API - ~A" strategy)))))
    
      ;; GET /:strategy/Player/setup
      ((and (string= method "GET")
            (ppcre:register-groups-bind (nil strategy) ("^/([^/]+)/Player/setup$" path)
              (declare (ignore nil))
              strategy))
       (let ((strategy (second (ppcre:split "/" path))))
         (if (valid-strategy-p strategy)
             (json-response
              `((name . "SimballBot")
                (primaryColor . ((jearsey . ,+primary-jersey-blue+)
                                 (pants . ,+primary-pants-white+)
                                 (socks . ,+primary-socks-blue+)))
                (secondaryColor . ((jearsey . ,+secondary-jersey-red+)
                                   (pants . ,+secondary-pants-black+)
                                   (socks . ,+secondary-socks-red+)))))
             (json-response '("error" . "Invalid strategy") 404))))

      ;; POST /:strategy/Player/update
      ((and (string= method "POST")
            (ppcre:register-groups-bind (nil strategy) ("^/([^/]+)/Player/update$" path)
              (declare (ignore nil))
              strategy))
       (let* ((strategy (second (ppcre:split "/" path))))
         (if (valid-strategy-p strategy)
             (let* ((raw-body (or (gethash "raw-body" req) "{}"))
                    (body (jonathan:parse raw-body))
                    (correlation-id (cdr (assoc "correlationId" body :test #'string=)))
                    (strat-key (intern (string-upcase strategy) :keyword))
                    (instructions (handle-update strat-key correlation-id body)))
               (when correlation-id
                 (save-history correlation-id body instructions))
               (json-response instructions))
             (json-response '("error" . "Invalid strategy") 404))))

      ;; Fallback: 404
      (t (json-response '(:error "Not found") 404))))))

;; För att starta servern: (simball-player:start-server)
(defun start-server (&key (port 5000))
  (clack:clackup #'app :port port))

;; --- Strategi-dispatch med CLOS ---
(defgeneric handle-update (strategy correlation-id body)
  (:documentation "Returnerar instruktioner för given strategi och request-body."))

(defmethod handle-update ((strategy (eql :default)) correlation-id body)
  '((p1Instructions . ((moveToX . 0) (moveToY . 0) (moveVelocity . 0)))
    (p2Instructions . ((moveToX . 0) (moveToY . 0) (moveVelocity . 0)))
    (p3Instructions . ((moveToX . 0) (moveToY . 0) (moveVelocity . 0)))
    (p4Instructions . ((moveToX . 0) (moveToY . 0) (moveVelocity . 0)))))

(defmethod handle-update ((strategy (eql :strategy1)) correlation-id body)
  ;; Exempel: returnera annan dummy-data
  '((p1Instructions . ((moveToX . 1) (moveToY . 1) (moveVelocity . 1)))
    (p2Instructions . ((moveToX . 2) (moveToY . 2) (moveVelocity . 2)))
    (p3Instructions . ((moveToX . 3) (moveToY . 3) (moveVelocity . 3)))
    (p4Instructions . ((moveToX . 4) (moveToY . 4) (moveVelocity . 4)))))

