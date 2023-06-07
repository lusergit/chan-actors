(load "procs.lisp")

(defun fork (take leave)
  (let ((master nil))
    (loop
     (if (not master)
	 ;; If no master is set, wait for a new master to take the fork
	 (let ((m (receive take)))
	   (setf master m))
	 ;; If a master is set, ask for the next leave message, if the
	 ;; sender is master, leave the fork
	 (let ((m (receive leave)))
	   (if (eq m master)
	       (setf master nil)))))))

(defun philosopher (id take-left take-right leave-left leave-right)
  (loop

   ;; Take resources
   (send take-left  id)
   (send take-right id)
   (format t "~s Eating~%" id)
   (sleep 3)

   ;; Leave resource
   (send leave-left  id)
   (send leave-right id)
   
   (format t "~s Done eating~%" id)
   (sleep 2)))


(let ((take0 (channel))			; Take channels
      (take1 (channel))
      (take2 (channel))
      (take3 (channel))
      
      (leave0 (channel))			;Leave channels
      (leave1 (channel))
      (leave2 (channel))
      (leave3 (channel)))
  ;; Forks
  (gofun fork take0 leave0)
  (gofun fork take1 leave1)
  (gofun fork take2 leave2)
  (gofun fork take3 leave3)

  ;; Philosophers
  (gofun philosopher :plato      take0 take1 leave0 leave1)
  (gofun philosopher :aristotele take1 take2 leave1 leave2)
  (gofun philosopher :socrate    take2 take3 leave2 leave3)
  (gofun philosopher :talete     take3 take0 leave3 leave0))
