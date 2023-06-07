(load "procs.lisp")

(defun work (s)
  (sleep s))

(defun CS (chan-money chan-coffee)
  (let ((wallet '(1 1 1 1 1 1)))	; The CS has a wallet with 6€
					; as 1€ coins
    (flet ((transform (coin)
	     (send chan-money coin)
	     (let ((x (receive chan-coffee)))
	       (format t
		       "CS: received ~d from ~d, energy filled up, working now...~%"
		       x
		       chan-coffee)
	       (work 1)
	       :pub)))
      (format t "CS: Starting with a wallet full of coins: ~d~%" wallet)
      (format t "CS: Successfully converted all my money in pubblications!~%~d"
	      (mapcar #'transform wallet)))))

(defun CM (chan-money chan-coffee)
  (loop
   (let ((money (receive chan-money)))
     (format t "CM: received ~d money~%" money)
     (if (>= money 1)
	 (send chan-coffee :coffee)
	 (format t "CM: received ~d money... Not enough!" money)))))

(defvar chan-money (channel))
(defvar chan-coffee (channel))

(gofun CM chan-money chan-coffee)
(gofun CS chan-money chan-coffee)
