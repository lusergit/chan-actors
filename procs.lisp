(ql:quickload :bordeaux-threads)

(defclass channel ()
  ((messages :initform '()
             :accessor messages
             :documentation "Messages in the channel")
   (lock :initform (bt:make-lock)
         :accessor lock
         :documentation
         "Lock to push/pop messages in the channel")
n   (cv :initarg :cv
       :initform (bt:make-condition-variable)
       :accessor cv
       :documentation
       "Condtional variable to notify the channel of a new message")))


;; send: non-blocking
;; x(y).P
(defmethod send ((self channel) &rest message)
  (with-slots (messages lock cv) self
    (bt:make-thread #'(lambda ()
                        (bt:with-lock-held (lock)
                          (setf messages (nconc messages (list message))))
                        (bt:condition-notify cv)))
    nil))


;; recive: blocking
;; 'x(y).P
(defmethod receive ((self channel))
  (with-slots (lock cv messages) self
    (loop
       (bt:with-lock-held (lock)
	 (if (not (null messages))
	     (return-from receive (car (pop messages)))
	     (bt:condition-wait cv lock))))))


;; Parallelism (P | Q)
(defmacro gofun (f &rest args)
  `(bt:make-thread
    (lambda () (apply #',f (list ,@args)))
    :name (concatenate 'string (string ',f) " worker")))


;; Utility functions
;; (v x).P
(defun channel ()
  (make-instance 'channel))


(defun smn (f &rest args)
  (lambda ()
    (apply f args)))


(defun pr (x)
  (print x *standard-output*)
  (format t "~%"))
