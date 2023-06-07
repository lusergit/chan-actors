(load "procs.lisp")

(defun reciever (chan name)
  (flet ((stop? (m) (equal m "stop")))
    (loop
      (let ((mex (receive chan)))
	(cond ((stop? mex) (return))
	      (t (format t "~s: got ~d~%" name mex)))))))

(defvar chan (channel))
(gofun reciever chan "rec1")
(gofun reciever chan "rec2")
(gofun reciever chan "rec3")
(mapcar (lambda (n) (send chan n)) '(1 2 3 4 5 6 7 8 9 10))
(mapcar (lambda (n) (send chan "stop")) '(1 2 3))
