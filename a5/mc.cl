; Author: Dor Rondel, Prof. Craig Graci
; Course: CSc 416

; Problem: Programming Challenge: Interactive Missionaries and Cannibals Problem Solving Framework

; Code Provided below until noted otherwise
( defun mc ()
    ( establish-world )
    ( init-move-list )
    ( make-moves )
)

( defun make-moves ()
    ( display-world )
    ( cond 
        (( goalp )
            ( write-line "good work!" )
            nil
        )
        (( feast-state-p )
            ( write-line "Yummy yummy yummy, I got Good in mt tummy!!")
            nil
        )
        ( t 
            ( let (( m (read)))
                ( if ( applicable-p m )
                    ( let () ( perform-move m ) ( make-moves ))
                    ( let () ( write-line "move inapplicable" ) nil )
                )
            )
        )
    )
)

( defun perform-move (move)
    ( setf *move-list* ( snoc move *move-list* ))
    ( if ( equal ( current-bank ) *left-bank*)
        ( move-lr move )
        ( move-rl move )
    )
)

( defun move-lr ( ml ) 
    ( if ( null ml ) ( return-from move-lr ))
    ( move-lr-1 ( first ml ))
    ( move-lr ( rest ml ))
)

( defun move-rl ( ml ) 
    ( if ( null ml ) ( return-from move-rl ))
    ( move-rl-1 ( first ml ))
    ( move-rl ( rest ml ))
)


; Code written par moi