;****************************
;Rhythm Constraints library version 1.0 by Ørjan Sandred, IRCAM 1999
;
;Update version 1.3 19/8 2002 (Stockholm)
;
;Updated/new functions in this document:
;  store-new-variable-in-vectors, change-pause2note, identic-within-layer-rule-wpauses, identic-between-layers-rule-wpauses, 
;  canon-rule-wpauses, global-canon-rule-wpauses, RC::gr-identical, RC::r-canon, RC::gr-canon, fast-band-filter2, fast-lp-filter2

(in-package RC)

;INTERFACE FOR INPUT OF RULES TO THE SEARCHENGINE

;**********PMC************

(defun rule-in-voice-pmc (voice rule)
  (list '* '?1 (list 'common-lisp-user::?if 
                     (list 'if (list '= '(get-voice-nr ?1) voice)
                       (list 'funcall rule '(1- (omcs::cur-index)) '?1)
                       't))))

(defun rules-in-voice-pmc (voice rules)
  (let ((rulelist (append '(and) (loop for rulex from 0 to (1- (length rules))
                                    collect (list 'funcall (nth rulex rules) '(1- (omcs::cur-index)) '?1)))))
    (list '* '?1 (list 'common-lisp-user::?if 
                     (list 'if (list '= '(get-voice-nr ?1) voice)
                        rulelist
                       't)))))

;special solution for pmc because that this engine first checks the whole domain, and then choose one
;answer (this makes the vector update impossible in the way situation works). It use the rl variable
;to access last correct answer CHOSEN BY THE ENGINE.
(defun call-rule-update-variables-all-voices-pmc (beat-grids)
  (list '* '?1 (list 'common-lisp-user::?if 
                     (list 'if '(> (omcs::cur-index) 1)
                           (list 'funcall (update-variables-all-voices beat-grids) 
                                 '(- (omcs::cur-index) 2) '(second common-lisp-user::rl))
                           't))))

;Rules for timesign subdiv send their info coded: ((list with beatvalues and allowed subdiv.) <function for rule>)
;This because the update vector rule needs this information. The collect-rules box pass the information.
(defun collect-rules-pmc (rulesvoice0 rulesvoice1 rulesvoice2 
                                      rulesvoice3 rulesvoice4 
                                      rulesvoice5 rulesvoice6)
  (let ((1beat-grid0 nil) (1beat-grid1 nil) (1beat-grid2 nil) (1beat-grid3 nil)
        (1beat-grid4 nil) (1beat-grid5 nil) (1beat-grid6 nil) rules)
    (setf rules
          (apply 'append
                 (if rulesvoice0 
                   (list (rules-in-voice-pmc 0 (loop for rulenr from 0 to (1- (length rulesvoice0))
                                               collect (if (typep (nth rulenr rulesvoice0) 'function)
                                                         (nth rulenr rulesvoice0)
                                                         (progn (setf 1beat-grid0 (append 1beat-grid0 (car (nth rulenr rulesvoice0))))
                                                                (cadr (nth rulenr rulesvoice0))))))))               
                 (if rulesvoice1 
                   (list (rules-in-voice-pmc 1 (loop for rulenr from 0 to (1- (length rulesvoice1))
                                               collect (if (typep (nth rulenr rulesvoice1) 'function)
                                                         (nth rulenr rulesvoice1)
                                                         (progn (setf 1beat-grid1 (append 1beat-grid1 (car (nth rulenr rulesvoice1))))
                                                                (cadr (nth rulenr rulesvoice1))))))))
                 (if rulesvoice2 
                   (list (rules-in-voice-pmc 2 (loop for rulenr from 0 to (1- (length rulesvoice2))
                                               collect (if (typep (nth rulenr rulesvoice2) 'function)
                                                         (nth rulenr rulesvoice2)
                                                         (progn (setf 1beat-grid2 (append 1beat-grid2 (car (nth rulenr rulesvoice2))))
                                                                (cadr (nth rulenr rulesvoice2))))))))
                 (if rulesvoice3 
                   (list (rules-in-voice-pmc 3 (loop for rulenr from 0 to (1- (length rulesvoice3))
                                               collect (if (typep (nth rulenr rulesvoice3) 'function)
                                                         (nth rulenr rulesvoice3)
                                                         (progn (setf 1beat-grid3 (append 1beat-grid3 (car (nth rulenr rulesvoice3))))
                                                                (cadr (nth rulenr rulesvoice3))))))))
                 (if rulesvoice4 
                   (list (rules-in-voice-pmc 4 (loop for rulenr from 0 to (1- (length rulesvoice4))
                                               collect (if (typep (nth rulenr rulesvoice4) 'function)
                                                         (nth rulenr rulesvoice4)
                                                         (progn (setf 1beat-grid4 (append 1beat-grid4 (car (nth rulenr rulesvoice4))))
                                                                (cadr (nth rulenr rulesvoice4))))))))
                 (if rulesvoice5 
                   (list (rules-in-voice-pmc 5 (loop for rulenr from 0 to (1- (length rulesvoice5))
                                               collect (if (typep (nth rulenr rulesvoice5) 'function)
                                                         (nth rulenr rulesvoice5)
                                                         (progn (setf 1beat-grid5 (append 1beat-grid5 (car (nth rulenr rulesvoice5))))
                                                                (cadr (nth rulenr rulesvoice5))))))))
                 (if rulesvoice6 
                   (list (rules-in-voice-pmc 6 (loop for rulenr from 0 to (1- (length rulesvoice6))
                                               collect (if (typep (nth rulenr rulesvoice6) 'function)
                                                         (nth rulenr rulesvoice6)
                                                         (progn (setf 1beat-grid6 (append 1beat-grid6 (car (nth rulenr rulesvoice6))))
                                                                (cadr (nth rulenr rulesvoice6))))))))))
    (append 
     (list (call-rule-update-variables-all-voices-pmc (list 1beat-grid0 1beat-grid1 1beat-grid2 1beat-grid3 
                                                      1beat-grid4 1beat-grid5 1beat-grid6)))
     rules)
    ))

;**********CSOLVER************
(defvar *situation_constraint_range* '(0_300))
;(setf *situation_constraint_range* '(0_100))
;(setf *situation_constraint_range* '(0_3))

(defun rule-in-voice (voice rule)
  (om::generic-cnstr

   #'(lambda (indexx indexy x y) ;could be changed when the Situation bug is fixed
       (declare (ignore indexy y))
       (if (= (get-voice-nr x) voice)
         (funcall rule indexx x)
         t))

   *situation_constraint_range*
   '(s 1 i)))

;*****************************************
; This rule make sure the vectors works!!
(defun call-rule-update-variables-all-voices (beat-grids)
       (om::generic-cnstr

   #'(lambda (indexx indexy x y) ;could be changed when the Situation bug is fixed
       (declare (ignore indexy y))
        (funcall (update-variables-all-voices beat-grids) 
                 indexx x))

   *situation_constraint_range*
   '(s 1 i)))
;*****************************************



;Rules for timesign subdiv send their info coded: ((list with beatvalues and allowed subdiv.) <function for rule>)
;This because the update vector rule needs this information. The collect-rules box pass the information.
;NEWER VERSION FURTHER DOWN!!! It is the way to create one rule for each voice that differs.
(defun collect-rules-*old-csolver* (rulesvoice0 rulesvoice1 rulesvoice2 
                                  rulesvoice3 rulesvoice4 
                                  rulesvoice5 rulesvoice6)
  (let ((1beat-grid0 nil) (1beat-grid1 nil) (1beat-grid2 nil) (1beat-grid3 nil)
        (1beat-grid4 nil) (1beat-grid5 nil) (1beat-grid6 nil) rules)
    (setf rules
          (apply 'append
                 (if rulesvoice0 (apply 'append
                                        (loop for rulenr from 0 to (1- (length rulesvoice0))
                                              collect (if (typep (nth rulenr rulesvoice0) 'function)
                                                        (rule-in-voice 0 (nth rulenr rulesvoice0))
                                                        (progn (setf 1beat-grid0 (append 1beat-grid0 (car (nth rulenr rulesvoice0))))
                                                               (rule-in-voice 0 (cadr (nth rulenr rulesvoice0))))))))
                 
                 (if rulesvoice1 (apply 'append
                                        (loop for rulenr from 0 to (1- (length rulesvoice1))
                                              collect (if (typep (nth rulenr rulesvoice1) 'function)
                                                        (rule-in-voice 1 (nth rulenr rulesvoice1))
                                                        (progn (setf 1beat-grid1 (append 1beat-grid1 (car (nth rulenr rulesvoice1))))
                                                               (rule-in-voice 1 (cadr (nth rulenr rulesvoice1))))))))
                 (if rulesvoice2 (apply 'append
                                        (loop for rulenr from 0 to (1- (length rulesvoice2))
                                              collect (if (typep (nth rulenr rulesvoice2) 'function)
                                                        (rule-in-voice 2 (nth rulenr rulesvoice2))
                                                        (progn (setf 1beat-grid2 (append 1beat-grid2 (car (nth rulenr rulesvoice2))))
                                                               (rule-in-voice 2 (cadr (nth rulenr rulesvoice2))))))))
                 (if rulesvoice3 (apply 'append
                                        (loop for rulenr from 0 to (1- (length rulesvoice3))
                                              collect (if (typep (nth rulenr rulesvoice3) 'function)
                                                        (rule-in-voice 3 (nth rulenr rulesvoice3))
                                                        (progn (setf 1beat-grid3 (append 1beat-grid3 (car (nth rulenr rulesvoice3))))
                                                               (rule-in-voice 3 (cadr (nth rulenr rulesvoice3))))))))
                 (if rulesvoice4 (apply 'append
                                        (loop for rulenr from 0 to (1- (length rulesvoice4))
                                              collect (if (typep (nth rulenr rulesvoice4) 'function)
                                                        (rule-in-voice 4 (nth rulenr rulesvoice4))
                                                        (progn (setf 1beat-grid4 (append 1beat-grid4 (car (nth rulenr rulesvoice4))))
                                                               (rule-in-voice 4 (cadr (nth rulenr rulesvoice4))))))))
                 (if rulesvoice5 (apply 'append
                                        (loop for rulenr from 0 to (1- (length rulesvoice5))
                                              collect (if (typep (nth rulenr rulesvoice5) 'function)
                                                        (rule-in-voice 5 (nth rulenr rulesvoice5))
                                                        (progn (setf 1beat-grid5 (append 1beat-grid5 (car (nth rulenr rulesvoice5))))
                                                               (rule-in-voice 5 (cadr (nth rulenr rulesvoice5))))))))
                 (if rulesvoice6 (apply 'append
                                        (loop for rulenr from 0 to (1- (length rulesvoice6))
                                              collect (if (typep (nth rulenr rulesvoice6) 'function)
                                                        (rule-in-voice 6 (nth rulenr rulesvoice6))
                                                        (progn (setf 1beat-grid6 (append 1beat-grid6 (car (nth rulenr rulesvoice6))))
                                                               (rule-in-voice 6 (cadr (nth rulenr rulesvoice6))))))))))
    (append 
     (call-rule-update-variables-all-voices (list 1beat-grid0 1beat-grid1 1beat-grid2 1beat-grid3 
                                                        1beat-grid4 1beat-grid5 1beat-grid6))
     rules)
    ))


(defun rules-in-voice (voice rules)
  (om::generic-cnstr
   
   #'(lambda (indexx indexy x y) ;could be changed when the Situation bug is fixed
       (declare (ignore indexy y))
       (if (= (get-voice-nr x) voice)
         (eval (append '(and) (loop for rulex from 0 to (1- (length rules))
                                    collect (funcall (nth rulex rules) indexx x))))
         t))
   
   *situation_constraint_range*
   '(s 1 i)))


(defun collect-rules (rulesvoice0 rulesvoice1 rulesvoice2 
                                  rulesvoice3 rulesvoice4 
                                  rulesvoice5 rulesvoice6)
  (let ((1beat-grid0 nil) (1beat-grid1 nil) (1beat-grid2 nil) (1beat-grid3 nil)
        (1beat-grid4 nil) (1beat-grid5 nil) (1beat-grid6 nil) rules)
    (setf rules
          (apply 'append
                 (if rulesvoice0 
                   (rules-in-voice 0 (loop for rulenr from 0 to (1- (length rulesvoice0))
                                           collect (if (typep (nth rulenr rulesvoice0) 'function)
                                                     (nth rulenr rulesvoice0)
                                                     (progn (setf 1beat-grid0 (append 1beat-grid0 (car (nth rulenr rulesvoice0))))
                                                            (cadr (nth rulenr rulesvoice0)))))))                 
                 (if rulesvoice1 
                   (rules-in-voice 1 (loop for rulenr from 0 to (1- (length rulesvoice1))
                                           collect (if (typep (nth rulenr rulesvoice1) 'function)
                                                     (nth rulenr rulesvoice1)
                                                     (progn (setf 1beat-grid1 (append 1beat-grid1 (car (nth rulenr rulesvoice1))))
                                                            (cadr (nth rulenr rulesvoice1)))))))
                 (if rulesvoice2 
                   (rules-in-voice 2 (loop for rulenr from 0 to (1- (length rulesvoice2))
                                           collect (if (typep (nth rulenr rulesvoice2) 'function)
                                                     (nth rulenr rulesvoice2)
                                                     (progn (setf 1beat-grid2 (append 1beat-grid2 (car (nth rulenr rulesvoice2))))
                                                            (cadr (nth rulenr rulesvoice2)))))))
                 (if rulesvoice3 
                   (rules-in-voice 3 (loop for rulenr from 0 to (1- (length rulesvoice3))
                                           collect (if (typep (nth rulenr rulesvoice3) 'function)
                                                     (nth rulenr rulesvoice3)
                                                     (progn (setf 1beat-grid3 (append 1beat-grid3 (car (nth rulenr rulesvoice3))))
                                                            (cadr (nth rulenr rulesvoice3)))))))
                 (if rulesvoice4 
                   (rules-in-voice 4 (loop for rulenr from 0 to (1- (length rulesvoice4))
                                           collect (if (typep (nth rulenr rulesvoice4) 'function)
                                                     (nth rulenr rulesvoice4)
                                                     (progn (setf 1beat-grid4 (append 1beat-grid4 (car (nth rulenr rulesvoice4))))
                                                            (cadr (nth rulenr rulesvoice4)))))))
                 (if rulesvoice5 
                   (rules-in-voice 5 (loop for rulenr from 0 to (1- (length rulesvoice5))
                                           collect (if (typep (nth rulenr rulesvoice5) 'function)
                                                     (nth rulenr rulesvoice5)
                                                     (progn (setf 1beat-grid5 (append 1beat-grid5 (car (nth rulenr rulesvoice5))))
                                                            (cadr (nth rulenr rulesvoice5)))))))
                 (if rulesvoice6 
                   (rules-in-voice 6 (loop for rulenr from 0 to (1- (length rulesvoice6))
                                           collect (if (typep (nth rulenr rulesvoice6) 'function)
                                                     (nth rulenr rulesvoice6)
                                                     (progn (setf 1beat-grid6 (append 1beat-grid6 (car (nth rulenr rulesvoice6))))
                                                            (cadr (nth rulenr rulesvoice6)))))))))
    (append 
     (call-rule-update-variables-all-voices (list 1beat-grid0 1beat-grid1 1beat-grid2 1beat-grid3 
                                                        1beat-grid4 1beat-grid5 1beat-grid6))
     rules)
    ))
;*******************************************
;SPECIAL RULE TO UPDATE TEMPORARY SOLUTION IN INTERNAL VECTORS; VERY IMPORTANT, THE HART OF THE SYSTEM!!!
;;;**********************UPDATE VECTORS RULE

(defun store-new-variable-in-vectors (indexx x beat-grid)
  (if (typep x 'rhythmcell)
    (let* ((this-voice-nr (get-voice-nr x))
           (this-layer-nr (get-layer-nr x))
           (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
           (this-global-cell (mapcar #'(lambda (onset) (+ start-this-cell onset))  (get-local-onset x)))
           (this-pause-flags (get-pauses x)))
      (put-this-rhythmcell this-global-cell this-pause-flags this-voice-nr this-layer-nr indexx))
    (let* ((this-voice-nr (get-voice-nr x))
           (this-layer-nr (get-layer-nr x))
           (this-time-sign (get-timesign x))
           (nr-of-beats (car this-time-sign))
           (beatvalue (cadr this-time-sign))
           (pointer-to-list 0)
           (1beat-test (progn (loop until (or (>= pointer-to-list (length beat-grid))
                                         (= beatvalue (car (nth pointer-to-list beat-grid))))                               
                               do (incf pointer-to-list))
                         (cadr (nth pointer-to-list beat-grid))))
           (1beat (if 1beat-test 1beat-test (list (/ 1 beatvalue)))) ;to avoid errors if the timesignature is not defined in r-beat-subdiv
           (beatlist-grid (apply 'append (make-list nr-of-beats :initial-element 1beat)))
           (start-this-measure (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
           (this-local-grid-abstime (om::dx->x start-this-measure beatlist-grid)))
      (put-this-timesign this-time-sign this-local-grid-abstime this-voice-nr this-layer-nr indexx)
      )))


(defun update-variables-all-voices (beat-grids)
  #'(lambda (indexx x)
      (let* ((this-layer-nr (get-layer-nr x))
             (this-voice-nr (get-voice-nr x)))
        
        (loop for voicenr from 0 to (1- *max-numberof-voices*)
              do (loop for layernr from 0 to *max-numberof-layers*
                       do (if (and (= this-layer-nr layernr) (= this-voice-nr voicenr))
                            (store-new-variable-in-vectors indexx x (nth voicenr beat-grids))
                            (step-fwd-pointers voicenr layernr indexx))))
        t))); This rule is always true! The purpose is only to update the vectors.

;****  R U L E S  *  R U L E S  *  R U L E S  *  R U L E S  *  R U L E S  *******

;*******************EQUAL LENGTH RULES

(defun equal-length-in-voice-rule (layer1 layer2) 
  
  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
              (stop-other-layer 0))
         
         (cond ((= this-layer-nr layer1)  ;compare lengths, case 1
                (setf stop-other-layer (get-stop-time this-voice-nr layer2 (1- indexx)))
                (if (<= start-this-cell stop-other-layer)
                  t
                  nil))
               ((= this-layer-nr layer2)  ;compare lengths, case 2
                (setf stop-other-layer (get-stop-time this-voice-nr layer1 (1- indexx)))
                (if (<= start-this-cell stop-other-layer)
                  t
                  nil))
               (t     ;bypass this rule
                t)
               ))
       )))

(defun equal-between-voices-rule (layer1 layer2 voice1 voice2) 
  
  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
              stop-other-layer)
         
         (cond ((and (= this-layer-nr layer1)
                     (= this-voice-nr voice1)) ;compare lengths, case 1
                (setf stop-other-layer (get-stop-time voice2 layer2 (1- indexx)))
                (if (<= start-this-cell stop-other-layer)
                  t
                  nil))
               ((and (= this-layer-nr layer2)
                     (= this-voice-nr voice2)) ;compare lengths, case 2
                (setf stop-other-layer (get-stop-time voice1 layer1 (1- indexx)))
                (if (<= start-this-cell stop-other-layer)
                  t
                  nil))
               (t     ;bypass this rule
                t)
               ))
       )))


(defun almost-equal-length-to-other-voice-rule (layer1 layer2 voice2 tolerance)
  
  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x)))
         
         (cond ((= this-layer-nr layer1)  ;compare lengths, case 1
                (let* ((this-voice-nr (get-voice-nr x))
                      (stop-other-layer (get-stop-time voice2 layer2 (1- indexx)))
                      (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx))))
                  (if (<= start-this-cell (+ stop-other-layer tolerance))
                    t
                    nil)))
                (t t) ;bypass this rule
                )
         ))))

;*****************HIERARCHY RULES
;filters with the side effect that it reverses the result
;input list has to be in order
(defun fast-band-filter (low high list)
  (member high (reverse (member low list :test '<=)) :test '>=))

(defun fast-band-filter2 (low high list)  ;;can handle pauses
  (member high (reverse (member low list :test #'(lambda (x y) (<= x (abs y))))) :test #'(lambda (x y) (>= x (abs y)))))

(defun fast-lp-filter (high list)
  (member high (reverse list)  :test '>=))

(defun fast-lp-filter2 (high list)  ;;can handle pauses
  (member high (reverse list)  :test #'(lambda (x y) (>= x (abs y)))))

(defun hierarchy-within-layer-rule (layerhigh layerlow) 
  
  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
              this-global-cell
              stop-this-cell
              other-layer-onset-times)
         
         (cond ((= this-layer-nr layerlow)           ;check hierarchy towards higher layer
                (setf other-layer-onset-times (get-one-rhythmlayer this-voice-nr layerhigh (1- indexx)))
                (setf this-global-cell (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x)))
                (setf stop-this-cell (+ start-this-cell (get-variabledur x)))
                (subsetp 
                 (fast-band-filter start-this-cell stop-this-cell other-layer-onset-times)
                 this-global-cell)
                )
               ((= this-layer-nr layerhigh)          ;check hierarchy towards lower layer
                (setf other-layer-onset-times (get-one-rhythmlayer this-voice-nr layerlow (1- indexx)))
                (setf this-global-cell (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x)))
                (subsetp 
                 (fast-lp-filter (get-stop-time this-voice-nr layerlow (1- indexx)) this-global-cell)
                 other-layer-onset-times)
                )
               (t t))                                ;the layer the variable belongs to is not included in this rule: bypass rule
         ))))


;New function - hierarchy where pauses are ignored.


(defun remove-pauses1 (proportion-list)
  (let ((result nil))
    (loop while proportion-list
          do (progn (if (and result
                             (< (first proportion-list) 0))
                      (setf (car (last result)) (+ (abs (car (last result))) (abs (car proportion-list))))
                      (setf result (append result (list (car proportion-list)))))
                    (pop proportion-list)))
    result))

(defun remove-pauses2 (onset-times)
  (remove nil (mapcar #'(lambda (timepoint) (if (< timepoint 0) nil timepoint)) onset-times)))



(defun hierarchy-within-layer-rule-ignore-pauses (layerhigh layerlow) 
  
  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              (start-this-cell (1+ (get-stop-time this-voice-nr this-layer-nr (1- indexx))))
              this-global-cell
              stop-this-cell
              other-layer-onset-times)
         
         (cond ((= this-layer-nr layerlow)           ;check hierarchy towards higher layer
                (setf other-layer-onset-times (remove-pauses2 (om::om* (om::om+ 1 (get-one-rhythmlayer this-voice-nr layerhigh (1- indexx)))
                                                                       (get-one-layer-pauseflags this-voice-nr layerhigh (1- indexx))
                                                                       )))
                (setf this-global-cell-with-pauses (om::om* (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x))
                                                            (get-pauses x)))
                (if (< (first this-global-cell-with-pauses) 0)
                  (setf this-global-cell (cdr (remove-pauses2 this-global-cell-with-pauses)))
                  (setf this-global-cell (remove-pauses2 this-global-cell-with-pauses)))
                
                (setf stop-this-cell (+ start-this-cell (get-variabledur x)))
                (subsetp 
                 (fast-band-filter2 start-this-cell stop-this-cell other-layer-onset-times)
                 this-global-cell)
                )
               ((= this-layer-nr layerhigh)          ;check hierarchy towards lower layer
                (setf other-layer-onset-times (remove-pauses2 (om::om* (om::om+ 1 (get-one-rhythmlayer this-voice-nr layerlow (1- indexx)))
                                                                       (append (get-one-layer-pauseflags this-voice-nr layerlow (1- indexx))
                                                                               '(1)))))
                (setf this-global-cell-with-pauses (om::om* (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x))
                                                            (get-pauses x)))
                (if (< (first this-global-cell-with-pauses) 0)
                  (setf this-global-cell (cdr (remove-pauses2 this-global-cell-with-pauses)))
                  (setf this-global-cell (remove-pauses2 this-global-cell-with-pauses)))
                
                (subsetp 
                 (fast-lp-filter2 (1+ (get-stop-time this-voice-nr layerlow (1- indexx))) this-global-cell)
                 other-layer-onset-times)
                )
               (t t))                                ;the layer the variable belongs to is not included in this rule: bypass rule
         ))))




(defun hierarchy-between-layers-rule (layerhigh layerlow voicehigh voicelow) 
  
  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
              this-global-cell
              stop-this-cell
              other-layer-onset-times)
         
         (cond ((and (= this-layer-nr layerlow)
                     (= this-voice-nr voicelow))  ;check hierarchy towards higher layer
                (setf other-layer-onset-times (get-one-rhythmlayer voicehigh layerhigh (1- indexx)))
                (setf this-global-cell (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x)))
                (setf stop-this-cell (+ start-this-cell (get-variabledur x)))
                (subsetp 
                 (fast-band-filter start-this-cell stop-this-cell other-layer-onset-times)
                 this-global-cell)
                )
               ((and (= this-layer-nr layerhigh)
                     (= this-voice-nr voicehigh))  ;check hierarchy towards lower layer
                (setf other-layer-onset-times (get-one-rhythmlayer voicelow layerlow (1- indexx)))
                (setf this-global-cell (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x)))
                (subsetp 
                 (fast-lp-filter (get-stop-time voicelow layerlow (1- indexx)) this-global-cell)
                 other-layer-onset-times)
                )
               (t t))                                ;the layer the variable belongs to is not included in this rule: bypass rule
         ))))


(defun hierarchy-between-layers-rule-ignore-pauses (layerhigh layerlow voicehigh voicelow) 
  
  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              (start-this-cell (1+ (get-stop-time this-voice-nr this-layer-nr (1- indexx))))
              this-global-cell
              stop-this-cell
              other-layer-onset-times)
         
         (cond ((and (= this-layer-nr layerlow)
                     (= this-voice-nr voicelow))  ;check hierarchy towards higher layer
                (setf other-layer-onset-times (remove-pauses2 (om::om* (om::om+ 1 (get-one-rhythmlayer voicehigh layerhigh (1- indexx)))
                                                                       (get-one-layer-pauseflags voicehigh layerhigh (1- indexx))
                                                                       )))
                (setf this-global-cell-with-pauses (om::om* (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x))
                                                            (get-pauses x)))
                (if (< (first this-global-cell-with-pauses) 0)
                  (setf this-global-cell (cdr (remove-pauses2 this-global-cell-with-pauses)))
                  (setf this-global-cell (remove-pauses2 this-global-cell-with-pauses)))
                
                (setf stop-this-cell (+ start-this-cell (get-variabledur x)))
                (subsetp 
                 (fast-band-filter2 start-this-cell stop-this-cell other-layer-onset-times)
                 this-global-cell)
                )
               ((and (= this-layer-nr layerhigh)
                     (= this-voice-nr voicehigh))          ;check hierarchy towards lower layer
                (setf other-layer-onset-times (remove-pauses2 (om::om* (om::om+ 1 (get-one-rhythmlayer voicelow layerlow (1- indexx)))
                                                                       (append (get-one-layer-pauseflags voicelow layerlow (1- indexx))
                                                                               '(1)))))
                (setf this-global-cell-with-pauses (om::om* (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x))
                                                            (get-pauses x)))
                (if (< (first this-global-cell-with-pauses) 0)
                  (setf this-global-cell (cdr (remove-pauses2 this-global-cell-with-pauses)))
                  (setf this-global-cell (remove-pauses2 this-global-cell-with-pauses)))
                
                (subsetp 
                 (fast-lp-filter2 (1+ (get-stop-time this-voice-nr layerlow (1- indexx))) this-global-cell)
                 other-layer-onset-times)
                )
               (t t))                                ;the layer the variable belongs to is not included in this rule: bypass rule
         ))))



;*****************HIEARARCHY to cellstarts rule

;OLD
(defun hierarchy-to-cellstart-within-layer-rule (layerhigh layerlow) 
  
  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
              this-global-cell
              stop-this-cell
              other-layer-onset-times
              other-layer-windowed-onset-times
              other-layer-cellstarts
              stop-other-layer
              this-global-cell-windowed)
         
         (cond ((= this-layer-nr layerlow)           ;check hierarchy towards higher layer
                (setf other-layer-onset-times (get-one-rhythmlayer this-voice-nr layerhigh (1- indexx)))
                (setf stop-this-cell (+ start-this-cell (get-variabledur x)))
                (setf other-layer-windowed-onset-times 
                      (reverse (fast-band-filter start-this-cell stop-this-cell other-layer-onset-times)))
                 (if (and other-layer-windowed-onset-times
                          (/= (car other-layer-windowed-onset-times) stop-this-cell))
                   (and 
                    (= start-this-cell (car other-layer-windowed-onset-times))
                    (not (cdr other-layer-windowed-onset-times)))
                   t)
                )
               ((= this-layer-nr layerhigh)          ;check hierarchy towards lower layer
                (setf this-global-cell (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x)))
                (setf stop-other-layer (get-stop-time this-voice-nr layerlow (1- indexx)))
                (setf this-global-cell-windowed 
                      (fast-lp-filter stop-other-layer this-global-cell))
                (setf other-layer-cellstarts (get-all-cell-startpoints-in-one-layer this-voice-nr layerlow (1- indexx)))

                (subsetp this-global-cell-windowed other-layer-cellstarts)
                )
               (t t)) ;the layer the variable belongs to is not included in this rule: bypass rule
         ))))


;;;NEW


(defun hierarchy-to-cellstart-within-layer-rule (layerhigh layerlow) 
  
  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
              this-global-cell
              stop-this-cell
              other-layer-onset-times
              other-layer-windowed-onset-times
              other-layer-cellstarts
              stop-other-layer
              this-global-cell-windowed)
         
         (cond ((= this-layer-nr layerlow)           ;check hierarchy towards higher layer
                (setf other-layer-onset-times (get-one-rhythmlayer this-voice-nr layerhigh (1- indexx)))
                (setf stop-this-cell (+ start-this-cell (get-variabledur x)))
                (setf other-layer-windowed-onset-times 
                      (reverse (fast-band-filter start-this-cell stop-this-cell other-layer-onset-times)))

                ;(print (list start-this-cell other-layer-onset-times other-layer-windowed-onset-times))
                (if other-layer-windowed-onset-times
                         
                  (and 
                   (= start-this-cell (car other-layer-windowed-onset-times))
                   (or (not (cdr other-layer-windowed-onset-times))
                       (= (cadr other-layer-windowed-onset-times)
                          stop-this-cell)))
                  t)
                )
               ((= this-layer-nr layerhigh)          ;check hierarchy towards lower layer
                (setf this-global-cell (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x)))
                (setf stop-other-layer (get-stop-time this-voice-nr layerlow (1- indexx)))
                (setf this-global-cell-windowed 
                      (fast-lp-filter stop-other-layer this-global-cell))
                (setf other-layer-cellstarts (append (get-all-cell-startpoints-in-one-layer this-voice-nr layerlow (1- indexx))
                                                     (list stop-other-layer)))
                (subsetp this-global-cell-windowed other-layer-cellstarts)

                )
               (t t)) ;the layer the variable belongs to is not included in this rule: bypass rule
         ))))



(defun hierarchy-to-cellstart-between-layers-rule (layerhigh layerlow voicehigh voicelow) 
  
  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
              this-global-cell
              stop-this-cell
              other-layer-onset-times
              other-layer-windowed-onset-times
              other-layer-cellstarts
              stop-other-layer
              this-global-cell-windowed)
         
         (cond ((and (= this-layer-nr layerlow) 
                    (= this-voice-nr voicelow))  ;check hierarchy towards higher layer
                (setf other-layer-onset-times (get-one-rhythmlayer voicehigh layerhigh (1- indexx)))
                (setf stop-this-cell (+ start-this-cell (get-variabledur x)))
                (setf other-layer-windowed-onset-times 
                      (reverse (fast-band-filter start-this-cell stop-this-cell other-layer-onset-times)))
                 (if (and other-layer-windowed-onset-times
                          (/= (car other-layer-windowed-onset-times) stop-this-cell))
                   (and 
                    (= start-this-cell (car other-layer-windowed-onset-times))
                    (not (cdr other-layer-windowed-onset-times)))
                   t)
                )
               ((and (= this-layer-nr layerhigh) 
                     (= this-voice-nr voicehigh))  ;check hierarchy towards lower layer
                (setf this-global-cell (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x)))
                (setf stop-other-layer (get-stop-time voicelow layerlow (1- indexx)))
                (setf this-global-cell-windowed 
                      (fast-lp-filter stop-other-layer this-global-cell))
                (setf other-layer-cellstarts (get-all-cell-startpoints-in-one-layer voicelow layerlow (1- indexx)))

                (subsetp this-global-cell-windowed other-layer-cellstarts)
                )
               (t t)) ;the layer the variable belongs to is not included in this rule: bypass rule
         ))))


;*****************IDENTITY RULES

(defun identic-within-layer-rule (layer1 layer2) 
  
  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              )
         
         (cond ((= this-layer-nr layer1)           ;check hierarchy towards higher layer
                (let* ((start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
                       (this-global-cell (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x)))
                       (stop-this-cell (+ start-this-cell (get-variabledur x)))
                       (other-layer-onset-times (get-one-rhythmlayer this-voice-nr layer2 (1- indexx)))
                       (stop-other-layer (get-stop-time this-voice-nr layer2 (1- indexx))))
                  (equal 
                   (fast-band-filter start-this-cell stop-this-cell other-layer-onset-times)
                   (fast-lp-filter stop-other-layer this-global-cell))
                  ))
               ((= this-layer-nr layer2)          ;check hierarchy towards lower layer
                (let* ((start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
                       (this-global-cell (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x)))
                       (stop-this-cell (+ start-this-cell (get-variabledur x)))
                       (other-layer-onset-times (get-one-rhythmlayer this-voice-nr layer1 (1- indexx)))
                       (stop-other-layer (get-stop-time this-voice-nr layer1 (1- indexx))))
                  (equal 
                   (fast-band-filter start-this-cell stop-this-cell other-layer-onset-times)
                   (fast-lp-filter stop-other-layer this-global-cell))
                  ))
               (t t))                                ;the layer the variable belongs to is not included in this rule: bypass rule
         ))))


(defun change-pause2note (cell timepoint)
  (mapcar #'(lambda (value) (if (= (abs value) timepoint) (abs value) value))
          cell))


(defun identic-within-layer-rule-wpauses (layer1 layer2) 
  
  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              )
         
         (cond ((= this-layer-nr layer1)           ;check hierarchy towards higher layer
                (let* ((start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
                       (this-global-cell (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x)))
                       (this-cell-pauses (append (get-pauses x) '(1)))
                       (stop-this-cell (+ start-this-cell (get-variabledur x)))
                       (other-layer-onset-times (get-one-rhythmlayer this-voice-nr layer2 (1- indexx)))
                       (other-layer-pause-flags (append (get-one-layer-pauseflags this-voice-nr layer2 (1- indexx)) '(1)))
                       (stop-other-layer (get-stop-time this-voice-nr layer2 (1- indexx))))
                  (equal 
                   (fast-band-filter2 (1+ start-this-cell) (1+ stop-this-cell) (om::om* other-layer-pause-flags (om::om+ 1 other-layer-onset-times)))
                   (change-pause2note (fast-lp-filter2 (1+ stop-other-layer) (om::om* (om::om+ 1 this-global-cell) this-cell-pauses))
                                      (1+ stop-other-layer)))
                  ))
               ((= this-layer-nr layer2)          ;check hierarchy towards lower layer
                (let* ((start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
                       (this-global-cell (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x)))
                       (this-cell-pauses (append (get-pauses x) '(1)))
                       (stop-this-cell (+ start-this-cell (get-variabledur x)))
                       (other-layer-onset-times (get-one-rhythmlayer this-voice-nr layer1 (1- indexx)))
                       (other-layer-pause-flags (append (get-one-layer-pauseflags this-voice-nr layer1 (1- indexx)) '(1)))
                       (stop-other-layer (get-stop-time this-voice-nr layer1 (1- indexx))))
                  (equal 
                   (fast-band-filter2 (1+ start-this-cell) (1+ stop-this-cell) (om::om* other-layer-pause-flags (om::om+ 1 other-layer-onset-times)))
                   (change-pause2note (fast-lp-filter2 (1+ stop-other-layer) (om::om* (om::om+ 1 this-global-cell) this-cell-pauses))
                                      (1+ stop-other-layer)))
                  ))
               (t t))                              ;the layer the variable belongs to is not included in this rule: bypass rule
         ))))


(defun identic-between-layers-rule-wpauses (layer1 layer2 voice1 voice2) 
  
  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              )
         
         (cond ((and (= this-layer-nr layer1)
                     (= this-voice-nr voice1))  ;check hierarchy towards higher layer
                (let* ((start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
                       (this-global-cell (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x)))
                       (this-cell-pauses (append (get-pauses x) '(1)))
                       (stop-this-cell (+ start-this-cell (get-variabledur x)))
                       (other-layer-onset-times (get-one-rhythmlayer voice2 layer2 (1- indexx)))
                       (other-layer-pause-flags (append (get-one-layer-pauseflags voice2 layer2 (1- indexx)) '(1)))
                       (stop-other-layer (get-stop-time voice2 layer2 (1- indexx))))
                  (equal 
                   (fast-band-filter2 (1+ start-this-cell) (1+ stop-this-cell) (om::om* other-layer-pause-flags (om::om+ 1 other-layer-onset-times)))
                   (change-pause2note (fast-lp-filter2 (1+ stop-other-layer) (om::om* (om::om+ 1 this-global-cell) this-cell-pauses))
                                      (1+ stop-other-layer)))
                  ))
               ((and (= this-layer-nr layer2)  
                     (= this-voice-nr voice2))  ;check hierarchy towards lower layer
                (let* ((start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
                       (this-global-cell (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x)))
                       (this-cell-pauses (append (get-pauses x) '(1)))
                       (stop-this-cell (+ start-this-cell (get-variabledur x)))
                       (other-layer-onset-times (get-one-rhythmlayer voice1 layer1 (1- indexx)))
                       (other-layer-pause-flags (append (get-one-layer-pauseflags voice1 layer1 (1- indexx)) '(1)))
                       (stop-other-layer (get-stop-time voice1 layer1 (1- indexx))))
                  (equal 
                   (fast-band-filter2 (1+ start-this-cell) (1+ stop-this-cell) (om::om* other-layer-pause-flags (om::om+ 1 other-layer-onset-times)))
                   (change-pause2note (fast-lp-filter2 (1+ stop-other-layer) (om::om* (om::om+ 1 this-global-cell) this-cell-pauses))
                                      (1+ stop-other-layer)))
                  ))
               (t t))                                ;the layer the variable belongs to is not included in this rule: bypass rule
         ))))

(defun identic-between-layers-rule (layer1 layer2 voice1 voice2) 
  
  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              )
         
         (cond ((and (= this-layer-nr layer1)
                     (= this-voice-nr voice1))  ;check hierarchy towards higher layer
                (let* ((start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
                       (this-global-cell (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x)))
                       (stop-this-cell (+ start-this-cell (get-variabledur x)))
                       (other-layer-onset-times (get-one-rhythmlayer voice2 layer2 (1- indexx)))
                       (stop-other-layer (get-stop-time voice2 layer2 (1- indexx))))
                  (equal 
                   (fast-band-filter start-this-cell stop-this-cell other-layer-onset-times)
                   (fast-lp-filter stop-other-layer this-global-cell))
                  ))
               ((and (= this-layer-nr layer2)  
                     (= this-voice-nr voice2))  ;check hierarchy towards lower layer
                (let* ((start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
                       (this-global-cell (mapcar #'(lambda (x) (+ start-this-cell x)) (get-local-onset x)))
                       (stop-this-cell (+ start-this-cell (get-variabledur x)))
                       (other-layer-onset-times (get-one-rhythmlayer voice1 layer1 (1- indexx)))
                       (stop-other-layer (get-stop-time voice1 layer1 (1- indexx))))
                  (equal 
                   (fast-band-filter start-this-cell stop-this-cell other-layer-onset-times)
                   (fast-lp-filter stop-other-layer this-global-cell))
                  ))
               (t t))                                ;the layer the variable belongs to is not included in this rule: bypass rule
         ))))

;*****************ORDER PRIORITY RULES

(defun order-priority-2layers (layer1 layer2) 
  
  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              (stop-layer1 0)
              (stop-layer2 0))
         
         (cond
          ((= this-layer-nr layer1)
           (let ((start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx))))
             (setf stop-layer1 (+ start-this-cell (get-variabledur x)))
             (setf stop-layer2 (get-stop-time this-voice-nr layer2 (1- indexx)))))
          ((= this-layer-nr layer2)
           (let ((start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx))))
             (setf stop-layer1 (get-stop-time this-voice-nr layer1 (1- indexx)))
             (setf stop-layer2 (+ start-this-cell (get-variabledur x))))))
         
         (>= stop-layer1 stop-layer2)
         )))
  )



(defun order-priority-3layers (layer1 layer2 layer3) 
  
  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              (stop-layer1 0)
              (stop-layer2 0)
              (stop-layer3 0))
         (cond
          ((= this-layer-nr layer1)
           (let ((start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx))))
             (setf stop-layer1 (+ start-this-cell (get-variabledur x)))
             (setf stop-layer2 (get-stop-time this-voice-nr layer2 (1- indexx)))
             (setf stop-layer3 (get-stop-time this-voice-nr layer3 (1- indexx)))))
          ((= this-layer-nr layer2)
           (let ((start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx))))
             (setf stop-layer1 (get-stop-time this-voice-nr layer1 (1- indexx)))
             (setf stop-layer2 (+ start-this-cell (get-variabledur x)))
             (setf stop-layer3 (get-stop-time this-voice-nr layer3 (1- indexx)))))
          ((= this-layer-nr layer3)
           (let ((start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx))))
             (setf stop-layer1 (get-stop-time this-voice-nr layer1 (1- indexx)))
             (setf stop-layer2 (get-stop-time this-voice-nr layer2 (1- indexx)))
             (setf stop-layer3 (+ start-this-cell (get-variabledur x))))))
         
         (>= stop-layer1 stop-layer2 stop-layer3)
         )))
  )


(defun order-priority-4layers (layer1 layer2 layer3 layer4) 
  
  (list
   #'(lambda (indexx x)
       (let* ((this-layer-nr (get-layer-nr x))
              (this-voice-nr (get-voice-nr x))
              (stop-layer1 0)
              (stop-layer2 0)
              (stop-layer3 0)
              (stop-layer4 0) )
         (cond
          ((= this-layer-nr layer1)
           (let ((start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx))))
             (setf stop-layer1 (+ start-this-cell (get-variabledur x)))
             (setf stop-layer2 (get-stop-time this-voice-nr layer2 (1- indexx)))
             (setf stop-layer3 (get-stop-time this-voice-nr layer3 (1- indexx)))
             (setf stop-layer4 (get-stop-time this-voice-nr layer4 (1- indexx)))))
          ((= this-layer-nr layer2)
           (let ((start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx))))
             (setf stop-layer1 (get-stop-time this-voice-nr layer1 (1- indexx)))
             (setf stop-layer2 (+ start-this-cell (get-variabledur x)))
             (setf stop-layer3 (get-stop-time this-voice-nr layer3 (1- indexx)))
             (setf stop-layer4 (get-stop-time this-voice-nr layer4 (1- indexx)))))
          ((= this-layer-nr layer3)
           (let ((start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx))))
             (setf stop-layer1 (get-stop-time this-voice-nr layer1 (1- indexx)))
             (setf stop-layer2 (get-stop-time this-voice-nr layer2 (1- indexx)))
             (setf stop-layer3 (+ start-this-cell (get-variabledur x)))
             (setf stop-layer4 (get-stop-time this-voice-nr layer4 (1- indexx)))))
          ((= this-layer-nr layer4)
           (let ((start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx))))
             (setf stop-layer1 (get-stop-time this-voice-nr layer1 (1- indexx)))
             (setf stop-layer2 (get-stop-time this-voice-nr layer2 (1- indexx)))
             (setf stop-layer3 (get-stop-time this-voice-nr layer3 (1- indexx)))
             (setf stop-layer4 (+ start-this-cell (get-variabledur x))))))
         
         (>= stop-layer1 stop-layer2 stop-layer3 stop-layer4)
         )))
  )

;;*****************CANON RULES

(defun canon-rule (layercomes layerdux offset) 
  
  (list
   #'(lambda (indexx x)
       (let ((this-layer-nr (get-layer-nr x)))
         
         (if (= this-layer-nr layercomes)
           (let* ((this-voice-nr (get-voice-nr x))
                  (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
                  (stop-this-cell (+ start-this-cell (get-variabledur x)))
                  (dux-onset-times (get-rhythm-within-timepoints this-voice-nr layerdux (1- indexx) 
                                                                 (- start-this-cell offset) 
                                                                 (- stop-this-cell offset)))
                  (comes-onset-times (mapcar #'(lambda (x) (- (+ start-this-cell x) offset)) (get-local-onset x))))
             (if (< start-this-cell offset)
               t
               (equal dux-onset-times comes-onset-times)))
           t) ;the layer the variable belongs to is not included in this rule: bypass rule
         ))))

(defun canon-rule-wpauses (layercomes layerdux offset) 
  
  (list
   #'(lambda (indexx x)
       (let ((this-layer-nr (get-layer-nr x)))
         
         (if (= this-layer-nr layercomes)
           (let* ((this-voice-nr (get-voice-nr x))
                  (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
                  (stop-this-cell (+ start-this-cell (get-variabledur x)))
                  (dux-onset-times (change-pause2note (om::om* (om::om+ 1 (get-rhythm-within-timepoints this-voice-nr layerdux (1- indexx) 
                                                                                                        (- start-this-cell offset) 
                                                                                                        (- stop-this-cell offset)))
                                                               (append (get-pauseflags-within-timepoints this-voice-nr layerdux (1- indexx) 
                                                                                                         (- start-this-cell offset) 
                                                                                                         (- stop-this-cell offset))
                                                                       '(1)))
                                                      (1+ (- stop-this-cell offset))))
                  (comes-onset-times (om::om* (om::om+ 1 (mapcar #'(lambda (x) (- (+ start-this-cell x) offset)) (get-local-onset x)))
                                              (append (get-pauses x) '(1)))))
             (if (< start-this-cell offset)
               t
               (equal dux-onset-times comes-onset-times)))
           t) ;the layer the variable belongs to is not included in this rule: bypass rule
         ))))

;;;;NEW canon rule
(defun canon-rule-wpauses-other-tempo (layercomes layerdux offset factor) 
  
  (list
   #'(lambda (indexx x)
       (let ((this-layer-nr (get-layer-nr x)))
         
         (if (= this-layer-nr layercomes)
           (let* ((this-voice-nr (get-voice-nr x))
                  (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
                  (stop-this-cell (+ start-this-cell (get-variabledur x)))
                  (dux-onset-times (change-pause2note (om::om* (om::om+ 1 (get-stretched-rhythm-within-timepoints this-voice-nr layerdux (1- indexx) 
                                                                                                                  (- start-this-cell offset) 
                                                                                                                  (- stop-this-cell offset)
                                                                                                                  factor))
                                                               (append (get-stretched-pauseflags-within-timepoints this-voice-nr layerdux (1- indexx) 
                                                                                                                   (- start-this-cell offset) 
                                                                                                                   (- stop-this-cell offset)
                                                                                                                   factor)
                                                                       '(1)))
                                                      (1+ (- stop-this-cell offset))))
                  (comes-onset-times (om::om* (om::om+ 1 (mapcar #'(lambda (x) (- (+ start-this-cell x) offset)) (get-local-onset x)))
                                              (append (get-pauses x) '(1)))))
             (if (< start-this-cell offset)
               t
               (equal dux-onset-times comes-onset-times)))
           t) ;the layer the variable belongs to is not included in this rule: bypass rule
         ))))



(defun global-canon-rule-wpauses (layercomes layerdux voicedux offset) 
  
  (list
   #'(lambda (indexx x)
       (let ((this-layer-nr (get-layer-nr x)))
         
         (if (= this-layer-nr layercomes)
           (let* ((this-voice-nr (get-voice-nr x))
                  (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
                  (stop-this-cell (+ start-this-cell (get-variabledur x)))
                  (dux-onset-times (change-pause2note (om::om* (om::om+ 1 (get-rhythm-within-timepoints voicedux layerdux (1- indexx) 
                                                                                                        (- start-this-cell offset) 
                                                                                                        (- stop-this-cell offset)))
                                                               (append (get-pauseflags-within-timepoints voicedux layerdux (1- indexx) 
                                                                                                         (- start-this-cell offset) 
                                                                                                         (- stop-this-cell offset))
                                                                       '(1)))
                                                      (1+ (- stop-this-cell offset))))
                  (comes-onset-times (om::om* (om::om+ 1 (mapcar #'(lambda (x) (- (+ start-this-cell x) offset)) (get-local-onset x)))
                                              (append (get-pauses x) '(1)))))
             (if (< start-this-cell offset)
               t
               (equal dux-onset-times comes-onset-times)))
           t) ;the layer the variable belongs to is not included in this rule: bypass rule
         ))))


;;;;NEW canon rule
(defun global-canon-rule-wpauses-other-tempo (layercomes layerdux voicedux offset factor) 
  
  (list
   #'(lambda (indexx x)
       (let ((this-layer-nr (get-layer-nr x)))
         
         (if (= this-layer-nr layercomes)
           (let* ((this-voice-nr (get-voice-nr x))
                  (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
                  (stop-this-cell (+ start-this-cell (get-variabledur x)))
                  (dux-onset-times (change-pause2note (om::om* (om::om+ 1 (get-stretched-rhythm-within-timepoints voicedux layerdux (1- indexx) 
                                                                                                                  (- start-this-cell offset) 
                                                                                                                  (- stop-this-cell offset)
                                                                                                                  factor))
                                                               (append (get-stretched-pauseflags-within-timepoints voicedux layerdux (1- indexx) 
                                                                                                                   (- start-this-cell offset) 
                                                                                                                   (- stop-this-cell offset)
                                                                                                                   factor)
                                                                       '(1)))
                                                      (1+ (- stop-this-cell offset))))
                  (comes-onset-times (om::om* (om::om+ 1 (mapcar #'(lambda (x) (- (+ start-this-cell x) offset)) (get-local-onset x)))
                                              (append (get-pauses x) '(1)))))
             (if (< start-this-cell offset)
               t
               (equal dux-onset-times comes-onset-times)))
           t) ;the layer the variable belongs to is not included in this rule: bypass rule
         ))))




(defun global-canon-rule (layercomes layerdux voicedux offset) 
  
  (list
   #'(lambda (indexx x)
       (let ((this-layer-nr (get-layer-nr x)))
         
         (if (= this-layer-nr layercomes)
           (let* ((this-voice-nr (get-voice-nr x))
                  (start-this-cell (get-stop-time this-voice-nr this-layer-nr (1- indexx)))
                  (stop-this-cell (+ start-this-cell (get-variabledur x)))
                  (dux-onset-times (get-rhythm-within-timepoints voicedux layerdux (1- indexx) 
                                                                 (- start-this-cell offset) 
                                                                 (- stop-this-cell offset)))
                  (comes-onset-times (mapcar #'(lambda (x) (- (+ start-this-cell x) offset)) (get-local-onset x))))
             (if (< start-this-cell offset)
               t
               (equal dux-onset-times comes-onset-times)))
           t) ;the layer the variable belongs to is not included in this rule: bypass rule
         ))))


;*********************
;om function

(om::defmethod! RC::r-eqlength  ((layer1 integer) 
                                 (layer2 integer))
   :initvals '(1 2)
   :indoc '("layernr" "layernr")
   :doc "Rule for forcing two layers in a voice to be of as equal length as possible.

<layer1> is the layer number for one of the layers.
<layer2> is the layer number for the other layer.

The rule will only accept a cell for the for the moment shortest 
layer as the next cell in the solution. Also works with layer 0 
(the time signatures).
--------------------------
Regel fšr att tvinga tvŒ skikt i en stŠmma att vara av sŒ lika lŠngd 
som mšjligt.

<layer1> Šr numret fšr ett av skikten.
<layer2> Šr numret fšr det andra skiktet.

Regeln accepterar endast en cell fšr det fšr tillfŠllet kortaste 
skiktet som nŠsta cell i lšsningen. Fungerar ocksŒ fšr skikt 0 
(taktarterna).
"

   :icon 352
   
   (equal-length-in-voice-rule layer1 layer2)
   )

(om::defmethod! RC::gr-eqlength  ((layer1 integer) 
                                  (voice1 integer)
                                  (layer2 integer)
                                  (voice2 integer))
   :initvals '(1 0 1 1)
   :indoc '("layernr" "voicenr" "layernr" "voicenr")
   :doc "Rule for forcing two layers in different voices to be of as equal length as possible.

<layer1> is the layer number for the first layers.
<voice1> is the voice number for the first layers.
<layer2> is the layer number for the second layer.
<voice2> is the voice number for the second layer.

The rule will only accept a cell for the for the moment shortest 
layer as the next cell in the solution. Also works with layer 0 
(the time signatures).

The rule has to be connected to the input for both layers on \Òrules->pmc\Ó.
--------------------------
Regel fšr att tvinga tvŒ skikt i olika stŠmmor att vara av sŒ lika lŠngd 
som mšjligt.

<layer1> Šr numret fšr det fšrsta skiktet.
<voice1> Šr numret pŒ stŠmman fšr det fšrsta skiktet.
<layer2> Šr numret fšr det andra skiktet.
<voice2> Šr numret pŒ stŠmman fšr det andra skiktet.

Regeln accepterar endast en cell fšr det fšr tillfŠllet kortaste 
skiktet som nŠsta cell i lšsningen. Fungerar ocksŒ fšr skikt 0 
(taktarterna).

Regeln mŒste kopplas till ingŒngen fšr de bŒda skikten pŒ \Òrules->pmc\Ó.
"
   :icon 352
   
   (equal-between-voices-rule layer1 layer2 voice1 voice2)
   )

(om::defmethod! RC::r-hierarchy ((layerhigh integer) 
                                 (layerlow integer)
                                 &optional (mode 'include-pauses)) 
   :initvals '(1 2 include-pauses)
   :indoc '("layernr" "layernr" "mode")
   :menuins '((2 (("include-pauses" 'include-pauses) ("ignore-pauses" 'ignore-pauses) ("cell-starts" 'cell-starts))))
   :doc "Rule for making a hierarchical connection between two layers in a voice.

<layerhigh> is the layer number for the higher layer in the hierarchy.
<layerlow> is the layer number for the lower layer in the hierarchy.
<mode> indicates if the hierarchy accepts any event in the lower layer 
(\Ònormal\Ó), or only starting points for the rhythm cells in the 
lower layer (\Òcell-starts\Ó).

The rule creates a hierarchical structure for events, where the higher 
layer only contains the events of \Òhigh importance\Ó (higher up in this 
structure) from the lower layer. Thus all starting points for events in 
the higher layer must exist in the lower layer. The lower layer might 
include events with starting points between starting points in the higher 
layer (= events of less importance).

\ÒCell-start\Ó mode is a stricter rule. Here the starting points in the 
higher layer has to occur at starting points for a whole cell in the 
lower layer (i.e. it is not enough that any event in the lower layer 
occur simultaneously as events in the higher layer).
--------------------------
Regel fšr att skapa en hierarkisk koppling mellan tvŒ skikt i en stŠmma.

<layerhigh> Šr numret fšr det hšgre skiktet i hierarkin.
<layerlow> Šr numret fšr det lŠgre skiktet i hierarkin.
<mode> indikerar om hierarkin gŠller fšr vilken som helst av hŠndelserna 
i det lŠgre skiktet (\Ònormal\Ó) eller endast accepterar startpunkter fšr 
rytmcellerna i det lŠgre skiktet (\Òcell-starts\Ó).

Regeln skapar en hierarkisk struktur fšr hŠndelser, dŠr det hšgre skiktet 
endast innehŒller hŠndelser av \Òstor vikt\Ó (hšgre upp i hierarkin) frŒn det 
lŠgre skiktet. SŒlunda mŒste alla hŠndelsers startpunkter i det hšgre skiktet 
existera i det lŠgre skiktet. Det lŠgre skiktet kan inkludera hŠndelser med 
startpunkt mellan startpunkterna i det hšgre skiktet (= hŠndelsr av mindre 
vikt).

\ÒCell-start\Ó mode Šr en striktare regel. HŠr mŒste starttiderna i det hšgre 
skiktet intrŠffa pŒ en starttid fšr en hel cell i det lŠgre skiktet (d.v.s. 
det rŠcker inte att vilken hŠndelse som helst i det lŠgre skiktet intrŠffar 
samtidigt som hŠndelserna i det hšgre skiktet).
"
   :icon 355
   
   (case mode
     ((include-pauses) (hierarchy-within-layer-rule layerhigh layerlow))
     ((ignore-pauses) (hierarchy-within-layer-rule-ignore-pauses layerhigh layerlow))
     ((cell-starts) (hierarchy-to-cellstart-within-layer-rule layerhigh layerlow))
     )
   )



(om::defmethod! RC::gr-hierarchy ((layerhigh integer)
                                  (voicehigh integer)
                                  (layerlow integer)
                                  (voicelow integer)
                                  &optional (mode 'include-pauses)) 
   :initvals '(1 0 1 1 include-pauses)
   :indoc '("layernr" "voicenr" "layernr" "voicenr" "mode")
   :menuins '((4 (("include-pauses" 'include-pauses) ("ignore-pauses" 'ignore-pauses) ("cell-starts" 'cell-starts))))
   :doc "Rule for making a hierarchical connection between two layers in different voices.

<layerhigh> is the layer number for the higher layer in the hierarchy.
<voicehigh> is the voice number for the higher layer in the hierarchy.
<layerlow> is the layer number for the lower layer in the hierarchy.
<voicelow> is the voice number for the lower layer in the hierarchy.
<mode> indicates if the hierarchy accepts any event in the lower layer 
(\Ònormal\Ó), or only starting points for the rhythm cells in the 
lower layer (\Òcell-starts\Ó).

The rule creates a hierarchical structure for events, where the higher 
layer only contains the events of \Òhigh importance\Ó (higher up in this 
structure) from the lower layer. Thus all starting points for events in 
the higher layer must exist in the lower layer. The lower layer might 
include events with starting points between starting points in the higher 
layer (= events of less importance).

\ÒCell-start\Ó mode is a stricter rule. Here the starting points in the 
higher layer has to occur at starting points for a whole cell in the 
lower layer (i.e. it is not enough that any event in the lower layer 
occur simultaneously as events in the higher layer).

The rule has to be connected to the input for both layers on \Òrules->pmc\Ó.
--------------------------
Regel fšr att skapa en hierarkisk koppling mellan tvŒ skikt i olika stŠmmor.

<layerhigh> Šr numret fšr det hšgre skiktet i hierarkin.
<voicehigh> Šr numret pŒ stŠmman fšr det hšgre skiktet i hierarkin.
<layerlow> Šr numret fšr det lŠgre skiktet i hierarkin.
<voicelow> Šr numret pŒ stŠmman fšr det lŠgre skiktet i hierarkin.
<mode> indikerar om hierarkin gŠller fšr vilken som helst av hŠndelserna 
i det lŠgre skiktet (\Ònormal\Ó) eller endast accepterar startpunkter fšr 
rytmcellerna i det lŠgre skiktet (\Òcell-starts\Ó).

Regeln skapar en hierarkisk struktur fšr hŠndelser, dŠr det hšgre skiktet 
endast innehŒller hŠndelser av \Òstor vikt\Ó (hšgre upp i hierarkin) frŒn det 
lŠgre skiktet. SŒlunda mŒste alla hŠndelsers startpunkter i det hšgre skiktet 
existera i det lŠgre skiktet. Det lŠgre skiktet kan inkludera hŠndelser med 
startpunkt mellan startpunkterna i det hšgre skiktet (= hŠndelsr av mindre 
vikt).

\ÒCell-start\Ó mode Šr en striktare regel. HŠr mŒste starttiderna i det hšgre 
skiktet intrŠffa pŒ en starttid fšr en hel cell i det lŠgre skiktet (d.v.s. 
det rŠcker inte att vilken hŠndelse som helst i det lŠgre skiktet intrŠffar 
samtidigt som hŠndelserna i det hšgre skiktet).

Regeln mŒste kopplas till ingŒngen fšr de bŒda skikten pŒ \Òrules->pmc\Ó.
"
   :icon 355
   
   (case (print mode)
     ((include-pauses) (hierarchy-between-layers-rule layerhigh layerlow voicehigh voicelow))
     ((ignore-pauses) (hierarchy-between-layers-rule-ignore-pauses layerhigh layerlow voicehigh voicelow))
     ((cell-starts) (hierarchy-to-cellstart-between-layers-rule layerhigh layerlow voicehigh voicelow))   
     )
   )


(om::defmethod! RC::r-identical  ((layer1 integer) 
                                  (layer2 integer)) 
   :initvals '(1 2)
   :indoc '("layernr" "layernr")
   :doc "Rule to force two layers in the same voice to be identical.

<layer1> is the layer number for one of the layers.
<layer2> is the layer number for the other layer.
--------------------------
Regel fšr att tvinga tvŒ skikt i samma stŠmma att vara identiska.

<layer1> Šr numret fšr ett av skikten.
<layer2> Šr numret fšr det andra skiktet.
"
   :icon 355
   
   (identic-within-layer-rule-wpauses layer1 layer2)
   )


(om::defmethod! RC::gr-identical  ((layer1 integer) 
                                   (voice1 integer)
                                   (layer2 integer)
                                   (voice2 integer)) 
   :initvals '(1 0 1 1)
   :indoc '("layernr" "voicenr" "layernr" "voicenr")
   :doc "Rule to force two layers in different voices to be identical.

<layer1> is the layer number for the first layers.
<voice1> is the voice number for the first layers.
<layer2> is the layer number for the second layer.
<voice2> is the voice number for the second layer.

The rule has to be connected to the input for both layers on \Òrules->pmc\Ó.
--------------------------
Regel fšr att tvinga tvŒ skikt i olika stŠmmor att vara identiska.

<layer1> Šr numret fšr det fšrsta skiktet.
<voice1> Šr numret pŒ stŠmman fšr det fšrsta skiktet.
<layer2> Šr numret fšr det andra skiktet.
<voice2> Šr numret pŒ stŠmman fšr det andra skiktet.

Regeln mŒste kopplas till ingŒngen fšr de bŒda skikten pŒ \Òrules->pmc\Ó.
"
   :icon 355
   
   (identic-between-layers-rule-wpauses layer1 layer2 voice1 voice2)
   )


(om::defmethod! RC::r-order-priority  ((layer1 integer)(layer2 integer) &optional layer3 layer4)

  :initvals '(1 2 3 4)
  :indoc '("layer1" "layer2" "layer3" "layer4")
  :doc "Rule to force the engine to find the solution looking at the layers in a specified order. 

This rule is experimental. It might speed up the search process (or might 
slow it down, if used in a wrong way). It might also help to find a specific 
answer, giving priority to certain layers. WARNING: The rule might forbid 
solutions that otherwise would have been possible.

<layer1> and <layer2> (optional can up to maximum 5 layers be specified) 
are layer numbers. They indicate what order (from left to right) the engine 
should look at the layers when searching for a solution.
--------------------------
Regel fšr att tvinga sškmotorn att titta pŒ skikten i en specificerad 
ordning nŠr den sšker efter en lšsningen.

Denna regel Šr experimentell. Den kan snabba upp sškprocessen (eller gšra 
den lŒngsammare om den Šr anvŠnd felaktigt). Den kan ocksŒ hjŠlpa till 
att hitta en speciell lšsning genom att ge prioritet till vissa skikt. 
VARNING: Regeln kan fšrbjuda lšsningar som annars skulle vara mšjliga.

<layer1> och <layer2> (alternativt kan upp till maximum fem skikt 
specificeras) Šr nummer pŒ skikt. De indikerar vilken ordning (frŒn 
vŠnster till hšger) sškmotorn ska titta pŒ dem dŒ den sšker efter en 
lšsning.
"
  :icon 352

  (cond 
   ((and layer3 layer4)
    (order-priority-4layers layer1 layer2 layer3 layer4))
   (layer3
    (order-priority-3layers layer1 layer2 layer3))
   (layer4
    (order-priority-3layers layer1 layer2 layer4))
   (t
    (order-priority-2layers layer1 layer2)))
  )


(om::defmethod! RC::r-canon  ((layercomes integer)
                              (layerdux integer)
                              (offset number)
                              &optional (tempo 1))
   :initvals '(2 1 0 1)
   :indoc '("layernr" "layernr" "duration" "factor")
   :doc "Rule to create a rhythmical canon.

<layercomes> is the layer number for the layer that will imitate the 
original rhythm.
<layerdux> is the layer number for the original rhythm.
<offset> is the distance in time (duration value as a ratio) between 
the original rhythm and its imitation.
<tempo> (optional) is a factor to scale the notevalues in the answer (i.e. comes).
WARNING! To use tempo-factors smaller than 1 in combination with the rule
r-eqlength might give no answer, since the dux does not exist when
comes is built. Use the r-layerorder instead of r-eqlength.


The layers have to be in the same voice (compare with \Ògr-canon\Ó).
--------------------------
Regel fšr att skapa en rytmisk kanon.

<layercomes> Šr  numret fšr det skikt som ska imitera orginalrytmen.
<layerdux> Šr numret fšr skiktet dŠr orginalrytmen finns.
<voicedux> Šr numret fšr stŠmman dŠr orginalrytmen finns.
<offset> Šr avstŒndet i tid (notvŠrde, angivet som ett brŒk) mellan 
orginalrytmen och imitationen.
<tempo> Šr en faktor fšr att skala notvŠrdena i svaret (comes).
VARNING! Om en tempofaktor mindre Šn 1 anvŠnds i kombination med regeln
r-eqlength kan det hŠnda att inget svar att hittas, dŒ dux inte existerar
nŠr comes byggs. AnvŠnd r-layerorder istŠllet fšr r-eqlength.

Skikten mŒste vara i samma stŠmma (jmfr \Ògr-canon\Ó).
"
   :icon 355
   
   (if (= 1 tempo)
     ;(canon-rule-wpauses layercomes layerdux offset) 
     (canon-rule-wpauses-other-tempo layercomes layerdux offset tempo)
     (canon-rule-wpauses-other-tempo layercomes layerdux offset tempo))
   )

(om::defmethod! RC::gr-canon  ((layercomes integer)
                               (layerdux integer)
                               (voicedux integer)
                               (offset number)
                               &optional (tempo 1))
   :initvals '(2 1 0 0 1)
   :indoc '("layernr" "layernr" "voicenr" "duration" "factor")
   :doc "Rule to create a rhythmical canon.

<layercomes> is the layer number for the layer that will imitate the 
original rhythm.
<layerdux> is the layer number for the original rhythm.
<voicedux> is the voice number for the original rhythm.
<offset> is the distance in time (duration value as a ratio) between 
the original rhythm and its imitation.
<tempo> (optional) is a factor to scale the notevalues in the answer (i.e. comes).
WARNING! To use tempo-factors smaller than 1 in combination with the rule
r-eqlength might give no answer, since the dux does not exist when
comes is built. Use the r-layerorder instead of r-eqlength.

The layers can be in different voices (compare with \Òr-canon\Ó). The rule 
should be connected to the voice where the imitation is (not the voice 
for the original rhythm).
--------------------------
Regel fšr att skapa en rytmisk kanon.

<layercomes> Šr  numret fšr det skikt som ska imitera orginalrytmen.
<layerdux> Šr numret fšr skiktet dŠr orginalrytmen finns.
<voicedux> Šr numret fšr stŠmman dŠr orginalrytmen finns.
<offset> Šr avstŒndet i tid (notvŠrde, angivet som ett brŒk) mellan 
orginalrytmen och imitationen.
<tempo> Šr en faktor fšr att skala notvŠrdena i svaret (comes).
VARNING! Om en tempofaktor mindre Šn 1 anvŠnds i kombination med regeln
r-eqlength kan det hŠnda att inget svar att hittas, dŒ dux inte existerar
nŠr comes byggs. AnvŠnd r-layerorder istŠllet fšr r-eqlength.

Skikten kan vara i olika stŠmmor (jmfr \Òr-canon\Ó). Regeln ska kopplas 
till den stŠmma dŠr imitationen finns (inte stŠmman dŠr orginalrytmen 
finns).
"
   :icon 355
   
   (if (= 1 tempo)
     (global-canon-rule-wpauses layercomes layerdux voicedux offset) 
     (global-canon-rule-wpauses-other-tempo layercomes layerdux voicedux offset tempo))
   )


(om::defmethod! RC::rules->Csolver  ((voice0 list)  &optional voice1 voice2 voice3 voice4 voice5 voice6)

  :initvals '(nil nil nil nil nil nil nil)
  :indoc '("rules" "rules" "rules" "rules" "rules" "rules" "rules")
  :doc "Format all rules for the Csolver <cnstr> input.

<voice0> is a list of all rules for voice number 0 (can be expanded 
up to maximum voice number 6).

A voice is identified by which entrance the box \Òvoice-domain\Ó 
is connected to on the \Òdomains->csolver\Ó box. Rules must be connected 
to the corresponding input on this box to be valid for the voice. 
See also \Òdomains->csolver\Ó.
--------------------------
Formatera alla regler fšr ingŒngen <cnstr> pŒ Csolver.

<voice0> Šr en lista med alla regler fšr stŠmma nummer 0 (kan 
expanderas upp till maxiamlt stŠmma nummer 6).

En stŠmma identifieras utifrŒn vilken ingŒng \Òvoice-domain\Ó Šr 
ansluten till pŒ \Òdomains->csolver\Ó. Regler mŒste vara anslutna till 
motsvarande ingŒng pŒ denna funktion fšr att gŠlla fšr en stŠmma. 
Se ocksŒ \Òdomains->csolver\Ó.
"
  :icon 365

  (collect-rules voice0 voice1 voice2 voice3 voice4 voice5 voice6)
   
  )


(om::defmethod! RC::rules->pmc  ((voice0 list)  &optional voice1 voice2 voice3 voice4 voice5 voice6)

  :initvals '(nil nil nil nil nil nil nil)
  :indoc '("rules" "rules" "rules" "rules" "rules" "rules" "rules")
  :doc "Format all rules for the pmc <rules> input.

<voice0> is a list of all rules for voice number 0 (can be expanded 
up to maximum voice number 6).

A voice is identified by which entrance the box \Òvoice-domain\Ó 
is connected to on the \Òdomains->pmc\Ó box. Rules must be connected 
to the corresponding input on this box to be valid for the voice. 
See also \Òdomains->pmc\Ó.
--------------------------
Formatera alla regler fšr ingŒngen <rules> pŒ pmc.

<voice0> Šr en lista med alla regler fšr stŠmma nummer 0 (kan 
expanderas upp till maxiamlt stŠmma nummer 6).

En stŠmma identifieras utifrŒn vilken ingŒng \Òvoice-domain\Ó Šr 
ansluten till pŒ \Òdomains->pmc\Ó. Regler mŒste vara anslutna till 
motsvarande ingŒng pŒ denna funktion fšr att gŠlla fšr en stŠmma. 
Se ocksŒ \Òdomains->pmc\Ó.
"
  :icon 375

  (collect-rules-pmc voice0 voice1 voice2 voice3 voice4 voice5 voice6)
   
  )

;********BACK COMPATIBILITY
(om::defmethod! RC::r-identity  ((layer1 integer) 
                                 (layer2 integer)) 
   :initvals '(1 2)
   :indoc '("layernr" "layernr")
   :doc "rule for makeing identical layers"
   :icon 355
   
   (identic-within-layer-rule layer1 layer2)
   )

(om::defmethod! RC::r-hierarchy-old  ((layerhigh integer) 
                                  (layerlow integer)) 
   :initvals '(1 2)
   :indoc '("layernr" "layernr")
   :doc "rule for creating hierarchy"
   :icon 355
   
   (hierarchy-within-layer-rule layerhigh layerlow)
   )

(om::defmethod! RC::r-hierarchy-to-cellstarts-old  ((layerhigh integer) 
                                                (layerlow integer)) 
   :initvals '(1 2)
   :indoc '("layernr" "layernr")
   :doc "Rule for creating hierarchy. Only cellstarts in the lower layer of the hierarchy is accepted,
not the events within a cell."
   :icon 355
   
   (hierarchy-to-cellstart-within-layer-rule layerhigh layerlow)
   )