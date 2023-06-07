(ql:quickload :bordeaux-threads)
(load "procs.lisp")

(defclass actor ()
  ((name :initarg :name
         :initform (error ":name must be specified")
         :accessor name
         :documentation "Hold the name of actor")
   (behavior :initarg :behavior
             :initform (error ":behav must be specified")
             :accessor behavior
             :documentation "Behavior")
   (queue :initarg :queue
	  :initform (channel)
	  :accessor queue
	  :documentation "Channel on which the actor reads incoming messages")
   (thread :accessor thread
	   :documentation "thread running the actor behavior")))


(defmethod send ((self actor) &rest message)
  (with-slots (queue) self
    (send queue message)))


(defmethod stop-actor ((self actor))
  "Stops the actor thread"
  (with-slots (thread) self
    (bt:destroy-thread  thread)))


(defmethod main ((self actor))
  (with-slots (queue (behav behavior)) self
    (loop
     (let ((mex (receive queue)))
       (setf behav (apply behav mex))
       (unless behav (return))))))


(defmethod initialize-instance :after ((self actor) &key)
  (with-slots (name thread) self
    (setf thread
          (bt:make-thread #'(lambda() (main self))
                          :name name))))


(defmacro defactor (name state vars &body body)
  `(defun ,name (&key (self) ,@state)
     (labels ((me ,(append vars `(&key (next #'me next-supplied-p)))
		(if next-supplied-p
                    (setf next (smn next :self self)))
                ,@body))
       (setf self (make-actor #'me ,(string name))))))

;; UTILS
(defun make-actor (behav name)
  (make-instance 'actor
                 :name (concatenate 'string "Actor: " name)
                 :behavior behav))
