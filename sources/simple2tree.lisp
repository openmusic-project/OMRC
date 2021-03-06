;****************************
;Rhythm Constraints library version 1.0 by Ørjan Sandred, IRCAM 1999
;


;Use the function simple->tree. All other functions are called from this one.
;This function builds a hierarchical rhythmtree from a sequence of rhythm values on the format
;(1/4 1/4 1/4 1/4).
;Function fuse-pauses from tree->simple

;In "better-predefined-subdiv?" you can define other way of notation for special cases (choise of subtrees).

 
(in-package RC) 

(defun make-proportional-cell (dur-list)
  (mapcar #'(lambda (dur) (* dur (apply 'lcm (mapcar 'denominator dur-list))))
          dur-list))

;offset from 1 always!!! Because otherwise the first pause ( -0!) will dissapear.
(defun x-dx-pause-ok (x-list)
  (mapcar '*
          (mapcar #'(lambda (absdur) (if (> absdur 0) 1 -1)) x-list)
          (om::x->dx (mapcar 'abs x-list))))

(defun dx-x-pause-ok (starttime x-list)
  (mapcar '*
          (append (mapcar #'(lambda (absdur) (if (> absdur 0) 1 -1)) x-list) '(1))
          (om::dx->x starttime (mapcar 'abs x-list))))

(defun simplify-proportions (proportion-list)
  (mapcar #'(lambda (value) (/ value (apply 'gcd proportion-list))) proportion-list))


;if a pause first, start list with -1
;if a tied note first, start list on next event (i.e. /= 1)
(defun make-sub-tree (local-onset)
  (list 1
        ;(fuse-pauses
         (cond ((and (or (= (car local-onset) 1) (= (car local-onset) -1)))
                (make-proportional-cell (x-dx-pause-ok local-onset)))
               (t
                (let ((proportional-list
                       (make-proportional-cell (x-dx-pause-ok (cons 1 local-onset)))))
                  (cons (float (first proportional-list))
                        (cdr proportional-list))))
               )))


(defun filter-events-between (start stop onsettimes)
  (let ((no-low-values (member start onsettimes :test #'(lambda (item value) (<= item (abs value))))))
    (reverse (member stop (reverse no-low-values) :test #'(lambda (item value) (>= item (abs value)))))))


(defun build-local-times (global-onsets global-start)
       (mapcar #'(lambda (onset) (if (> onset 0)
                                   (- onset (1- global-start))
                                   (+ onset (1- global-start))))
               global-onsets))


(defun create-beat (global-onset global-start beat-length)
  (let ((local-onset (build-local-times global-onset global-start))
        tree)
    (if (not local-onset) (setf local-onset (list (1+ beat-length))))
    (if (= (car (last local-onset)) (- -1 beat-length))
      (setf local-onset (append (butlast local-onset) (list (1+ beat-length)))))
    (if (/= (car (last local-onset)) (1+ beat-length))
      (setf local-onset (append local-onset (list (1+ beat-length)))))
    (setf tree (better-predefined-subdiv? (make-sub-tree local-onset)))
    (if (= (length (cadr tree)) 1)
      (caadr tree)
      tree)))


    
(defun fuse-pauses-and-tied-notes-between-beats (measure-tree no-of-beats)
  (let ((beat-nr 0))
    (loop until (>=  beat-nr no-of-beats)
          collect (cond ((and (typep (nth beat-nr measure-tree) 'number)
                              (< (nth beat-nr measure-tree) 0))
                         (let ((value (nth beat-nr measure-tree)))
                           (incf beat-nr)
                           (loop until (or (typep (nth beat-nr measure-tree) 'list)
                                           (>=  beat-nr no-of-beats)
                                           (and (typep (nth beat-nr measure-tree) 'integer)
                                                (> (nth beat-nr measure-tree) 0)))
                                 do (progn (decf value (truncate (abs (nth beat-nr measure-tree))))
                                           (incf beat-nr)))
                           value))
                        ((and (typep (nth beat-nr measure-tree) 'number))
                         (let ((value (nth beat-nr measure-tree)))
                           (incf beat-nr)
                           (loop until (not (typep (nth beat-nr measure-tree) 'float))
                                 do (progn (incf value (truncate (nth beat-nr measure-tree)))
                                           (incf beat-nr)))
                           value))
                        (t (incf beat-nr)
                           (nth (1- beat-nr) measure-tree))))
    ))



(defun build-one-measure (local-onset no-of-beats beat-length)
  (let ((beatlist (om::dx->x 1 (make-list no-of-beats :initial-element beat-length)))
        tree)
    (setf tree
          (fuse-pauses-and-tied-notes-between-beats
           (loop for beat-nr from 0 to (- (length beatlist) 2)
                 collect (let ((these-events (filter-events-between (nth beat-nr beatlist)
                                                                    (nth (1+ beat-nr) beatlist)
                                                                    local-onset)))
                           ;check if tied pause within subtree - if yes: give startpoint as pause
                           (if (and these-events
                                    (/= (abs (first these-events)) (nth beat-nr beatlist))
                                    (get-onsettime-before (nth beat-nr beatlist) local-onset)
                                    (> 0 (get-onsettime-before (nth beat-nr beatlist) local-onset)))
                             (setf these-events (append (list (- 0 (nth beat-nr beatlist)))
                                                        these-events))) 
                           ;check if tied pause within subtree - if yes: give startpoint as pause
                           (if (and (not these-events)
                                    (get-onsettime-before (nth beat-nr beatlist) local-onset)
                                    (> 0 (get-onsettime-before (nth beat-nr beatlist) local-onset)))
                             (setf these-events (list (- 0 (nth beat-nr beatlist)))))
                           
                           (create-beat these-events (nth beat-nr beatlist) beat-length)))
           no-of-beats))
    (list (list no-of-beats (/ 1 beat-length)) tree)))



(defun get-onsettime-before (timepoint abs-rhythm)
  (car (member timepoint (reverse abs-rhythm) :test #'(lambda (item value) (> item (abs value))))))


(defun buildmeasure-seq (abs-rhythms timesigns)
  (let ((measure-start-points (om::dx->x 1 (mapcar #'(lambda (timesign) (apply '/ timesign)) timesigns))))
    (loop for measure from 0 to (1- (length timesigns))
          collect (let ((this-seq (filter-events-between (nth measure measure-start-points)
                                                         (nth (1+ measure) measure-start-points)
                                                         abs-rhythms))
                        (this-timesign (nth measure timesigns))
                        local-onset)
                    ;check if measure starts with tied pause
                    (if (and this-seq
                             (/= (abs (first this-seq)) (nth measure measure-start-points))
                             (> 0 (get-onsettime-before (nth measure measure-start-points) abs-rhythms)))
                      (setf this-seq (append (list (- 0 (nth measure measure-start-points)))
                                             this-seq)))
                    (if (and (not this-seq) 
                             (> 0 (get-onsettime-before (nth measure measure-start-points) abs-rhythms)))
                      (setf this-seq (list (- 0 (nth measure measure-start-points)))))
                    (setf local-onset (build-local-times this-seq (nth measure measure-start-points)))
                    
                    (build-one-measure local-onset 
                                       (car this-timesign)
                                       (/ 1 (cadr this-timesign)))))))

(defun fuse-pauses2 (proportion-list)
  (let ((result nil))
    (loop while proportion-list
          do (progn (if (and result
                             (< (first proportion-list) 0)
                             (< (car (last result)) 0))
                      (setf (car (last result)) (+ (car (last result)) (car proportion-list)))
                      (setf result (append result (list (car proportion-list)))))
                    (pop proportion-list)))
    result))

(defun simple->tree (rhythmseq timesignseq)
  (let ((abs-rhythms (dx-x-pause-ok 1 (fuse-pauses2 (append rhythmseq '(-100))))))
    (list '? (buildmeasure-seq abs-rhythms timesignseq))))

;(defun simple->tree (rhythmseq timesignseq)
;  (let ((abs-rhythms (dx-x-pause-ok 1 (append rhythmseq '(-100)))))
;    (list '? (buildmeasure-seq abs-rhythms timesignseq))))

(defun better-predefined-subdiv? (sub-tree)
  (let* ((proportional-list (cadr sub-tree))
        (pauses (mapcar #'(lambda (value) (if (< value 0) -1 1)) proportional-list))
        (abs-proportional-list (mapcar 'abs proportional-list))
        abs-answer)
    (setf abs-answer
          (cond ((equal abs-proportional-list '(2 2 2 3 3))
                 (list (list 2 (list (first pauses)(second pauses)(third pauses)))(fourth pauses)(fifth pauses)))
                ((equal abs-proportional-list '(3 3 2 2 2))
                 (list (first pauses)(second pauses)(list 2 (list (third pauses)(fourth pauses)(fifth pauses)))))
                ((equal abs-proportional-list '(3 2 2 2 3))
                 (list (first pauses)(list 2 (list (second pauses)(third pauses)(fourth pauses)))(fifth pauses)))
                ((equal abs-proportional-list '(3.0 2 2 2 3))
                 (list (coerce (first pauses) 'float)(list 2 (list (second pauses)(third pauses)(fourth pauses)))(fifth pauses)))
                ((equal abs-proportional-list '(3 3 4 2))
                 (list (first pauses)(second pauses)(list 2 (list (* 2 (third pauses))(fourth pauses)))))
                ((equal abs-proportional-list '(4 2 3 3))
                 (list (list 2 (list (* 2 (first pauses))(second pauses)))(third pauses)(fourth pauses)))
                ((equal abs-proportional-list '(2 4 3 3))
                 (list (list 2 (list (first pauses)(* 2 (second pauses))))(third pauses)(fourth pauses)))
                ((equal abs-proportional-list '(3 3 2 4))
                 (list (first pauses)(second pauses)(list 2 (list (third pauses)(* 2 (fourth pauses))))))
                ((equal abs-proportional-list '(3 1 1 1))
                 (list (first pauses)(list 1 (list (second pauses)(third pauses)(fourth pauses)))))
                ((equal abs-proportional-list '(3.0 1 1 1))
                 (list (coerce (first pauses) 'float)(list 1 (list (second pauses)(third pauses)(fourth pauses)))))
                ((equal abs-proportional-list '(1 1 1 3))
                 (list (list 1 (list (first pauses)(second pauses)(third pauses)))(fourth pauses)))
                (t proportional-list)))
    (list 1 abs-answer)))


;************************
;om function
(om::defmethod! RC::simpleformat->tree  ((rhythm list) 
                                                (timesigns list)
                                         ) 
  :initvals '((1/4 1/4 1/4 1/4) (4 4))
  :indoc '("rhythm" "timesigns")
  :doc "Convert a list of note values to a hierarchical rhythm-tree.

<rhythm> is a list of note values as ratios.
<timesigns> is either a list of time signatures, or one time 
signature.

If a list of time signatures is given, this sequence will be used. 
If only one time signature is given, this will be used as many times 
as needed to notate the sequence.
--------------------------
Konvertera en lista av notv_rden till ett hierarkiskt rytmtr_d.

<rhythm> _r en lista av notv_rden som br_k.
<timesigns> _r antingen en lista av taktartssignaturer, eller en 
taktartssignatur.

Om en lista av taktartssignaturer ges kommer denna sekvens at anv_ndas. 
Om endast en taktartssignatur ges kommer denna att anv_ndas s_ m_nga 
g_nger som beh_vs f_r att notera sekvensen.
"
  :icon 351
  
  (if (typep (car timesigns) 'list)
      (simple->tree  rhythm timesigns)
      (let ((nr-of-measures (* (/ (apply '+ (mapcar 'abs rhythm)) (car timesigns)) (cadr timesigns))))
        (if (= (rem nr-of-measures 1) 0)
          (simple->tree rhythm (make-list (truncate nr-of-measures) :initial-element timesigns))
          (simple->tree rhythm (make-list (1+ (truncate nr-of-measures)) :initial-element timesigns))))
  ))

