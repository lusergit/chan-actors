(load "actors.lisp")

(defactor computer/scientist ((pubs 0) (money 3) coffeem name) (m)
  (flet ((coffee? (c) (equal c "coffee"))
	 (start? (m) (equal m "start"))
	 (notenough? (p) (<= p 0))
	 (donation? (m) (and (listp m)
			     (equal (first m) "donation")
			     (numberp (second m))))
	 (fire? (m) (equal m "You're fired!"))
	 (get-donation (m) (second m))
	 (work (n) (sleep n)))
    (cond
      ((donation? m)
       (incf money (get-donation m))
       (format t "~s:~t thanks for the donation of ~d€!~%" name (get-donation m))
       next)

      ((fire? m)
       (format t "~d: Quitting the job...~%" name))
      
      ((notenough? money)
       (format t "~s:~t Successfully converted all my money in pubblications!~%" name)
       next)
      
      ((or (coffee? m) (start? m))
       (format t "~s:~t Working on something new ... ~%" name)
       (work 3)
       (incf pubs)
       (format t
	       "~s:~t Made ~d pubblications in total, need some coffee... ~%"
	       name pubs)
       (decf money)
       (send coffeem `(1 ,self))
       next)
      
      (t
       (format t "~s:~t I wanted some coffee.. but instead got ~s ~%" name m)
       next))))

(defactor coffee/machine () (robj)
  (flet ((get-money (a) (first a))
	 (get-customer (a) (second a)))
    (cond ((>= (get-money robj) 1)
	   (progn
	     (send (get-customer robj) "coffee")
	     (format t "COFFEEM:~t outputted some coffee to ~d~%" (name (get-customer robj)))))
	  (t (format t "COFFEEM:~t Not enough money: received ~d$ ~%" (get-money robj))))
    next))

(defvar machine (coffee/machine))
(defvar cs (computer/scientist :coffeem machine
			       :name 'luca))
;; (send cs "start")
;; (send cs '("donation" 3))
;; (send machine `(1 ,cs))
;; (send cs "You're fired!")
