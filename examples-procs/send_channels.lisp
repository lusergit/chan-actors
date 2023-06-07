(load "procs.lisp")

(defvar chan-of-chan (channel))

(defun receiver (ch)
  (format t "RECEIVER: Init")
  (let ((x (receive ch)))
    (format t "RECEIVER: received channel ~d~%" x))
  (format t "RECEIVER: Gracefully dying"))

(defvar chan-to-send (channel))

(gofun receiver chan-of-chan)

(format t "MAIN: sending channel ~d~%" chan-to-send)
(send chan-of-chan chan-to-send)
