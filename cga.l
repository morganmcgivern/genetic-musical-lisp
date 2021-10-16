;morgan mcgivern 2020
(load 'lp.l)

(defun random-note(scale)
  ;select a random note from scale scale
  (nth (random (length scale)) scale)
  )

(defun rgl (l c scale)
  ;recursively generate list
    (if (< 0 c)
      (cons (random-note scale) (rgl l (- c 1) scale))
      (cons (random-note scale) nil))
  )

(defun note-string(scale)
  ;string of 16 notes
  (rgl '() *string-length* scale)
  )

( defmethod mutation ( ( note-str list ) &aux position symbol )
  ( setf position ( random ( length note-str ) ) )
  ( setf symbol ( others *scale* ( nth position note-str ) ) )
  ( change note-str ( pick symbol ) position )
  )

(defmethod others (symbols elem)
  (remove elem symbols)
)

(defun change (l elem pos)
(let ((output (copy-list l)))
  (setf (nth pos output) elem)
  output)
)

(defmethod crossover (l1 l2)
  ;crosses over two lists
  (mapcar #'(lambda (a b) (if (= 1 (random 2)) b a)) l1 l2) )

(defmethod fitness-scale (l scale)
    ;how many are in scale
    (let* ((count 0))
      (mapcar #'(lambda (x) (if (eq (car (member x scale)) x) (incf count))) l)
      count)
  )

(defun sum-occurances (l i)
  (let* ((count 0))
    (mapcar #'(lambda (x) (if (eq i x) (incf count))) l)
    count)
  )

(defmethod fitness-key (l)
  ;5*(1st tone sum of durations) + 1*(3rd tone) + 3 *(5th tone) / sum of all durations
  (float (/ (+ (* 5 (sum-occurances l (first *fit-scale*))) (* 1 (sum-occurances l (third *fit-scale*))) (* 3 (sum-occurances l (fifth *fit-scale*)))) (- (length l) 1)))
  )



( defclass individual ()
  (
   ( note-string :accessor individual-note-string :initarg :note-string )
   ( fitness :accessor individual-fitness :initarg :fitness )
   ( number :accessor individual-number :initarg :number )
  )
)

( defmethod random-individual (&aux notes)
  ( setf notes ( note-string c-maj) )
  ( make-instance 'individual
                  :note-string notes
                  :fitness ( funcall *fitness* notes)
                  :number 0
                  )
  )

( defmethod new-individual ( ( nr number ) ( notes list ) )
  ( make-instance 'individual
                  :note-string notes
                  :fitness ( funcall *fitness* notes)
                  :number nr
                  )
  )

( defmethod display ( ( i individual ) )
  ( display-nnl i ) ( terpri )
  )

( defmethod display-nnl ( ( i individual ) )
  ( prin1 ( individual-number i ) )
  ( princ ( filler ( individual-number i ) ) )
  ( prin1 ( individual-note-string i ) )
  ( princ " " )
  ( prin1 ( individual-fitness i ) )
  ( princ ( filler ( individual-fitness i ) ) )
  )

( defmethod filler ( ( n number ) )
  ( cond
    ( ( < n 10 ) " " )
    ( ( < n 100 ) " " )
    ( ( < n 1000 ) " " )
    ( ( < n 10000 ) " " )
    ( ( < n 100000 ) " " )
    )
  )

; ( defmethod fitness-b ( ( i individual ) )
;   ( fitness-b ( individual-rbg-string i ) )
;   )

( defmethod fitness-key ( ( i individual ))
  ( fitness-key ( individual-note-string i ))
  )

; ( defmethod individual-demo (&aux i0 i1 i2 i3 one two three) ( setf *fitness* #'fitness-key )
;   ( setf i0 ( random-individual ) )
;   ( display i0 )
;   ( setf one ( note-string c-maj) )
;   ( setf i1 ( new-individual 1 one ) )
;   ( display i1 )
;   ( setf two ( note-string c-maj) )
;   ( setf i2 ( new-individual 2 two ) )
;   ( display i2 )
;   ( setf three ( note-string c-maj) )
;   ( setf i3 ( new-individual 3 three ) )
;   ( display i3 )
;
;   ( format t "Fitness of i0 = ~A~%" ( funcall *fitness* i0 c-maj) ) ( format t "Fitness of i1 = ~A~%" ( funcall *fitness* i1 c-maj) ) ( format t "Fitness of i2 = ~A~%" ( funcall *fitness* i2 c-maj) ) ( format t "Fitness of i3 = ~A~%" ( funcall *fitness* i3 c-maj) ) nil
;   )


( defclass population ()
  (
   ( individuals :accessor population-individuals :initarg :individuals )
   ( generation :accessor population-generation :initform 0 )
   )
  )
( defmethod size ( ( p population ) )
  ( length ( population-individuals p ) )
)

( defmethod display ( ( p population ) )
  ( terpri ) ( terpri )
  ( princ "Generation " )
  ( prin1 ( population-generation p ) )
  ( princ " population ..." )
  ( terpri ) ( terpri )
  ( dolist ( i ( population-individuals p ) )
           ( display i )
           )
  ( terpri )
  )

( defmethod initial-population ( &aux individuals )
  ( setf individuals () )
  ( dotimes ( i *population-size* )
    ( push ( new-individual ( + i 1 ) ( note-string *scale*) ) individuals )
  )
  ( make-instance 'population :individuals ( reverse individuals ) )
)

( defmethod average ( ( p population ) &aux ( out 0 ) )
  ( dolist ( i ( population-individuals p ) )
           ( setf out (+ out (individual-fitness i)) )
           )
  (float (/ out (size p)))
  )

( defmethod initial-population ( &aux individuals )
  ( setf individuals () )
  ( dotimes ( i *population-size* )
    ( push ( new-individual ( + i 1 ) ( note-string *scale*) ) individuals )
  )
  ( make-instance 'population :individuals ( reverse individuals ) )
)

( defmethod average ( ( p population ) &aux ( out 0 ) )
  ( dolist ( i ( population-individuals p ) )
           ( setf out (+ out (individual-fitness i)) )
           )
  (float (/ out (size p)))
  )

( setf *select-demo* nil )

( defmethod select-individual ( ( p population ) &aux i candidates rn )
  ( setf candidates ( select-individuals p ) )
  ( setf mfi ( most-fit-individual candidates ) )
  ( if *select-demo* ( select-demo-helper candidates mfi) )
  mfi
)

( defmethod select-individuals ( ( p population ) &aux individuals candidates rn )
  ( setf individuals ( population-individuals p ) )
  ( setf candidates () )
  ( dotimes ( i *selection-size* )
  ( setf rn ( random *population-size* ) )
  ( push ( nth rn individuals ) candidates )
  )
  candidates
)

( defmethod most-fit-individual ( ( l list ) &aux (max-value 0) max-individual )
    (car (sort l #'> :key #'individual-fitness))
  )

( defmethod select-demo-helper ( ( l list ) ( i individual ) ) ( princ "the sample of individuals ..." ) ( terpri )
  ( mapcar #'display l )
  ( terpri )
  ( princ "the most fit of the sample ... " ) ( terpri )
  ( display i )
  ( terpri )
  nil
)

( defmethod population-demo (&aux p)
  ( setf p ( initial-population ) )
  ( display p )
  ( format t "Average fitness = ~A~%~%" ( average p ) )
  ( setf *select-demo* t )
  ( format t "Sampling ...~%~%" )
  ( select-individual p ) ( terpri)
  ( format t "Sampling ...~%~%" )
  ( select-individual p ) ( terpri)
  ( format t "Sampling ...~%~%" )
  ( select-individual p ) ( terpri)
  )

( defmethod mutate ( ( i individual ) &aux mutation )
  ( setf mutation ( mutation ( individual-note-string i ) ) )
  ( make-instance 'individual
  :number ( individual-number i ) :note-string mutation
  :fitness ( funcall *fitness* mutation)
  ) )


( defmethod maybe-mutate ( ( i individual ) )
  ( if ( <= ( + 1 ( random 100 ) ) *pc-m* )
    ( mutate i )
    i )
  )

( defmethod mutate-demo ()
  ( setf i ( random-individual ) )
  ( display i )
  ( dotimes ( x 20 )
  ( setf i ( mutate i ) )
  ( display i ) )
  )

( defmethod maybe-mutate-demo ()
  ( setf i ( random-individual ) )
  ( display i )
  ( dotimes ( x 20 )
            ( setf n ( maybe-mutate i ) )
            ( display-nnl n )
            ( if ( not ( equal n i ) ) ( princ " *" ) ) ( terpri )
            ( setf i n )
            ) )

;task 9

( setf *copy-demo* nil )

( defconstant *pc-c* 40 )

( defmethod perform-copies ( ( cp population ) ( np population ) )
  ( dotimes ( i ( nr-copies ) )
    ( perform-one-copy cp np )
  )
)

( defmethod nr-copies ()
  ( * ( / *pc-c* 100 ) *population-size* )
)

( defmethod perform-one-copy ( ( cp population ) ( np population ) &aux x m mm new-i )
  ( setf m ( select-individual cp ) )
  ( if *copy-demo* ( format t "Selected individual = ~%" ) )
  ( if *copy-demo* ( display m ) )
  ( setf mm ( maybe-mutate m ) )
  ( if *copy-demo* ( format t "Possibly muted individual = ~&" ) )
  ( if *copy-demo* ( display mm ) )
  ( setf ( individual-number mm ) ( + 1 ( size np ) ) )
  ( if *copy-demo* ( format t "Renumbered individual = ~&" ) )
  ( if *copy-demo* ( display mm ) )
  ( setf new-i ( new-individual ( + 1 ( size np ) ) ( individual-note-string mm ) ) ) ( setf
                                                                                      ( population-individuals np )
                                                                                      ( append ( population-individuals np ) ( list new-i ) ) )
  nil )
( defmethod empty-population ( ( cp population ) &aux np )
( setf np ( make-instance 'population ) )
( setf ( population-individuals np ) () )
( setf ( population-generation np ) ( + 1 ( population-generation cp ) ) )

np )

( defmethod perform-copies-demo ( &aux cp np )
  ( setf cp ( initial-population ) )
  ( setf np ( empty-population cp ) )
  ( format t "-------------------------------------------------------------------------~%~%~%" )
  ( display np )
  ( format t "~%~%-------------------------------------------------------------------------~%" )
  ( setf *select-demo* t )
  ( setf *copy-demo* t )
  ( dotimes ( i 10 )
            ( perform-one-copy cp np )
            ( format t "-------------------------------------------------------------------------~%~%~%" ) ( display np )
            ( format t "~%~%-------------------------------------------------------------------------~%" )
            )
  ( setf *select-demo* nil )
  ( setf *copy-demo* nil ) nil
  )
( setf *crossover-demo* nil )

( defconstant *pc-x* 60 )

( defmethod perform-crossovers ( ( cp population ) ( np population ) )
  ( dotimes ( i ( nr-crossovers ) )
    ( perform-one-crossover cp np ) )
)

( defmethod nr-crossovers ()
  ( * ( / *pc-x* 100 ) *population-size* )
)


( defmethod perform-one-crossover ( ( cp population ) ( np population ) )
  ( let ( x m mm mother father new-i )
    ( setf mother ( select-individual cp ) )
    ( setf father ( select-individual cp ) )
    ( if *crossover-demo* ( format t "Selected mother = ~%" ) )
    ( if *crossover-demo* ( display mother ) )
    ( if *crossover-demo* ( format t "Selected father = ~&" ) )
    ( if *crossover-demo* ( display father ) )
    ( setf m ( crossover mother father ) )
    ( if *crossover-demo* ( format t "the crossover = ~&" ) )
    ( if *crossover-demo* ( display m ) )
    ( setf mm ( maybe-mutate m ) )
    ( if *crossover-demo* ( format t "the possiblly mutated individual = ~&" ) )
    ( if *crossover-demo* ( display mm ) )
    ( setf ( individual-number mm ) ( + 1 ( size np ) ) )
    ( if *crossover-demo* ( format t "the renumbered individual = ~&" ) )
    ( if *crossover-demo* ( display mm ) )
    ( setf new-i ( new-individual ( + 1 ( size np ) ) ( individual-note-string mm ) ) )
    ( setf
      ( population-individuals np )
      ( append ( population-individuals np ) ( list new-i ) )
    )
  )
  nil )

( defmethod crossover ( ( mother individual ) ( father individual ) &aux mi fi x i )
  ( setf mi (individual-note-string mother ) )
  ( setf fi (individual-note-string father ) )
  ( setf x ( crossover mi fi ) )
  ( setf i ( new-individual 0 x ) )
i )

( defmethod perform-crossovers-demo ( &aux cp np )
  ( setf cp ( initial-population ) )
  ( setf np ( empty-population cp ) )
  ( format t "-------------------------------------------------------------------------~%~%~%" )
  ( display np )
  ( format t "~%~%-------------------------------------------------------------------------~%" )
  ( setf *select-demo* t )
  ( setf *crossover-demo* t )
  ( dotimes ( i 10 )
            ( perform-one-crossover cp np )
            ( format t "-------------------------------------------------------------------------~%~%~%" ) ( display np )
            ( format t "~%~%-------------------------------------------------------------------------~%" )
            )
  ( setf *select-demo* nil )
  ( setf *crossover-demo* nil ) nil
  )

( defmethod next-generation ( ( cp population ) &aux np )
  ( setf np ( empty-population cp ) )
  ( perform-copies cp np )
  ( perform-crossovers cp np )
  np
  )

( setf *nr-generations* 5 )

( defmethod ga ( &aux p )
  ( format t "THE WORLD IS C MAJOR ~%~%" )
  ( setf *fit-scale* c-maj )
  ( setf p ( initial-population ) ) ( terpri )
  ( summarize p )
  ( dotimes ( i *nr-generations* )
    ( setf p ( next-generation p ) )
    ( check-average p )
            )
  ( terpri )
  ( summarize p )
  ( format t "The world is c major chromatic ~%~%" )
  ( setf *fit-scale* c-maj-chrom )
  ( dotimes ( i *nr-generations* )
    ( setf p ( next-generation p ) )
    ( check-average p )
  )
  ( terpri )
  ( summarize p )
  ( format t "THE WORLD IS E FLAT ~%~%" )
  ( setf *fit-scale* eb-maj )
  ( dotimes ( i *nr-generations* )
    ( setf p ( next-generation p ) )
    ( check-average p )
  )
  ( terpri )
  ( summarize p )
)

( defmethod summarize ( ( p population ) ) ( display p )
  ( check-average p )
  ( terpri )
)

( defmethod check-average ( ( p population ) )
  ( format t "average fitness of population ~A = ~A~%"
           ( population-generation p )
           ( average p ) )
  )

; (defmethod cga ( &aux p )
;   ( format t "THE WORLD IS C MAJOR ~%~%" )
;   ( setf *fit-scale* c-maj )
;   ( setf p ( initial-population ) ) ( terpri )
;   ( summarize p )
;   ( dotimes ( i 5 )
;     ( setf p ( next-generation p ) )
;     ( check-average p )
;             )
;   ( terpri )
;   ( summarize p )
;   )
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;  (_,.....,_)
;    |||||||
;    |||||||
;    |||||||
;    |||||||  hello and welcome
;    |||||||  ascii art cause i'm bored
;    |||||||
;    |||||||
;   '_______'
;    }     {
;   /       \
; _/_________\_
;|_____________|
(defmethod fitness-user (l)
  (format t "Melody is ~S~%" l)
  (format t "rate 1-10~%")
  (setf userfit (read))
  (/ userfit 10)
)

(defmethod fitness-blues (l)
  ; C F C C
  ; F F C C
  ; G F C G
  ; every four notes should conform to that
  ; each note must be a half or whole step away from it's following note
  (setf cbluesroot
    '(C x x x F x x x C x x x C x x x
      F x x x F x x x C x x x C x x x
      G x x x F x x x C x x x G x x x)
    )
  ;check the roots
  (setf rootcount 0)
  (mapcar #'(lambda (x y) (if (and (eq x y) (not (eq y 'x))) (incf rootcount))) l cbluesroot)
  (setf rootfitness (float (/ rootcount 12)))
  ;each note should be one half or whole step away to the next note
  ;each x should become +1 or -1 from it's surrounding notes
  (setf index -1)
  (setf goodnotes 0)
  (mapcar #'(lambda (x)
                    (cond
                      ((is-within-step x (nth (+ index 1) l)) (incf goodnotes)) ;note after note x is within one step or one whole step of note x
                      ((not (eq x (nth (+ index 1) l))) (incf goodnotes)) ;duplicates aren't great most of the time
                      )
                    (incf index)
                    ) l)
  (setf goodnotesfitness (float (/ goodnotes (* *string-length* .75))))
  (+ rootfitness goodnotesfitness)
)

(defun is-within-step (a b)
  ;note a is within one whole or half step of note b
  ;find the location of note a, then if b is within two chromatic notes of a it is good
  (< (abs (- (position a c-maj-chrom) (position b c-maj-chrom))) 2)
  )

;settings
( defconstant *population-size* 50 )
( defconstant *selection-size* 10 )
( defconstant *pc-m* 50 )
( setf *fitness* #'fitness-blues ) ;fitness functions take only 1 argument, the list
( setf *string-length* 48)
; ^sharp _flat
(setf pitches '(c (c# db) d (d# eb) e f (f# gb) g (g# ab) a (a# bb) b c))
;(setf c-maj-chrom '(c c# d d# e f g g# a a# b c))
(setf c-maj-chrom '(c ^c d ^d e f g ^g a ^a b c))
(setf c-maj '(c d e f g a b c))
(setf eb-maj '(eb f g ab bb c d eb))
(setf *fit-scale* c-maj) ;scale fitness is being tested against
(setf *scale* c-maj-chrom) ;what notes can be randomly generated




(defmethod generatemusic (&aux p)
  ( format t "Starting... ~%" )
  ( setf p ( initial-population ) ) ( terpri )
  ( summarize p )
  ( format t "How many generations? ~%" )
  (setf *nr-generations* (read))
  ( dotimes ( i *nr-generations* )
    ( setf p ( next-generation p ) )
    ( check-average p )
  )
  ( terpri )
  ( summarize p )
  )
