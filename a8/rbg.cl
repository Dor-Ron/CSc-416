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
(defmethod crossover (( m list) (f list))
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

