; Author: Dor Rondel
; Course: CSc-416

; Problem: RBG GA

(setf *limit* 25)

; TASK 1

; Randomally returns either r, b, or g
(defmethod rbg () 
    (nth (random 3) '(R B G))
)

; Generates an RBG list of size len
(defmethod rbg-list ((len integer)) 
    (cond 
        ((<= len 0)
            (princ "Length must have a positive polarity")
            nil
        
        )
        ((= len 1)
            (list (rbg))
        )
        ((> len 1) 
            (cons 
                (rbg) 
                (rbg-list (- len 1))
            )
        )
    )
)

; Generates rbg list of length limit
(defmethod rbg-string ()
    (rbg-list *limit*)
)

; TASK 2
; Removes symbol from list
(defmethod others ((lst list) (sym symbol)) 
    (remove sym lst)
)

; Returns random element from list
(defmethod pick ((lst list))
    (nth (random (length lst)) lst)
)

; Replces 2 arg for 3 arg from list in first arg
(defmethod change ((lst list)  (new symbol) (pos integer) &aux tmp)
    (cond
        ((< pos 0 ) 
            (princ "Must enter natural integer index")
            nil
        )
        ((= pos 0)
            (cons new (rest lst))
        )
        ((> pos 0)
            (cons (first lst) (change (rest lst) new (- pos 1)))
        )
    )
)

; supplied method, applies mutation on single base of string
(defmethod mutation ((rbg-str list) &aux p q)
    (setf p (random (length rbg-str)))
    (setf q (others '(r b g) (nth p rbg-str)))
    (change rbg-str (pick q) p)
)

; TASK 3
; Makes a list of first pos elements of list m
(defmethod first-n ((m list) (pos integer))
   (cond
        ((< pos 0 ) 
            (princ "Must enter natural integer index")
            nil
        )
        ((= pos 0)
            '()
        )
        ((> pos 0)
            (cons (first m) (first-n (rest m) (- pos 1)))
        )
    ) 
)

; Makes list of all elements occuring after pos index of m
(defmethod rest-n ((m list) (pos integer))
   (cond
        ((< pos 0 ) 
            (princ "Must enter natural integer index")
            nil
        )
        ((= pos 0)
            m
        )
        ((> pos 0)
            (rest-n (rest m) (- pos 1))
        )
    ) 
)

; Given method, applies crossover to rbg string
(defmethod crossover ((m list) (f list))
    (setf pos (+ 1 (random (length m))))
    (append (first-n m pos) (rest-n f pos))
)

; TASK 4

; Supplied method, demonstrates mutation behind the scence action
(defmethod mutation-demo (&aux s m)
  (setf s (rbg-string))
  (dotimes (i 10)
    (format t "s = ~A~%" s)
    (setf m (mutation s))
    (format t "m = ~A~%~%" m)
  )
)

; Supplied method, demonstrates on crossover behind the scence action
(defmethod crossover-demo (&aux m f x)
  (setf m (rbg-string))
  (setf f (rbg-string))
  (dotimes (i 10)
    (format t "m = ~A~%" m)
    (setf x (crossover m f))
    (format t "x  = ~A~%" x)
    (format t "f = ~A~%~%" f)
  )
)

; TASK 5

(defmethod fitness-r ((lst list))
    (count 'R lst)
)

(defmethod fitness-g ((lst list))
    (count 'G lst)
)

(defmethod fitness-b ((lst list))
    (count 'B lst)
)

; Provided method to demonstrate different fitness methods
(defmethod fitness-demo (&aux rbg-str fitness)
  (setf rbg-str (rbg-string))
  (format t "rbg-str = ~A~%" rbg-str)
  (format t "Directly applying the fitness metrics ...~%")
  (format t "fitness-r = ~A~%" (fitness-r rbg-str))
  (format t "fitness-b = ~A~%" (fitness-b rbg-str))
  (format t "fitness-g = ~A~%" (fitness-g rbg-str))
  (format t "Indirectly applying the fitness metrics ...~%")
  (setf fitness #'fitness-r)
  (format t "fitness-r = ~A~%" (funcall fitness rbg-str))
  (setf fitness #'fitness-b)
  (format t "fitness-b = ~A~%" (funcall fitness rbg-str))
  (setf fitness #'fitness-g)
  (format t "fitness-g = ~A~%" (funcall fitness rbg-str))
)
 
 ; TASK 6
 (defclass individual ()
	(
		(rbg-string :accessor individual-rbg-string :initarg :rbg-string)
		(fitness :accessor individual-fitness :initarg :fitness)
		(number :accessor individual-number :initarg :number)
	)
)

(defmethod random-individual (&aux rbg) 
	(setf rbg (rbg-string))
	(make-instance 'individual
		:rbg-string rbg
		:fitness (funcall *fitness* rbg)
		:number 0 
	)	
)

(defmethod new-individual ((nr number) (notes list))
	(make-instance 'individual
		:rbg-string notes
		:fitness (funcall *fitness* notes)
		:number nr
	)	
)

(defmethod display ((i individual))
	(display-nnl i) (terpri)
)

(defmethod display-nnl ((i individual))
	(prin1 (individual-number i))
	(princ (filler (individual-number i)))
	(prin1 (individual-rbg-string i))
	(princ "  ")
	(prin1 (individual-fitness i))
	(princ (filler (individual-fitness i)))
)

(defmethod filler ((n number))
	(cond
		((< n 10) "     ")
		((< n 100) "    ")
		((< n 1000) "   ")
		((< n 10000) "  ")
		((< n 100000) " ")
	)
)

(defmethod fitness-b ((i individual))
	(fitness-b (individual-rbg-string i))
)

(defmethod fitness-r ((i individual))
	(fitness-r (individual-rbg-string i))
)	

(defmethod fitness-g ((i individual))
	(fitness-g (individual-rbg-string i))
)

(defmethod individual-demo (&aux i0 i1 i2 i3 one two three)
	(setf *fitness* #'fitness-r)
	(setf i0 (random-individual))
	(display i0)
	(setf one (rbg-string))
	(setf i1 (new-individual 1 one))
	(display i1)
	(setf two (rbg-string))
	(setf i2 (new-individual 2 two))
	(display i2)
	(setf three (rbg-string))
	(setf i3 (new-individual 3 three))
	(display i3)
	(format t "Fitness of i0 = ~A~%" (funcall *fitness* i0))
	(format t "Fitness of i1 = ~A~%" (funcall *fitness* i1))
	(format t "Fitness of i2 = ~A~%" (funcall *fitness* i2))
	(format t "Fitness of i3 = ~A~%" (funcall *fitness* i3))
	nil
)

; TASK 7