;****************************
;Rhythm Constraints library version 1.0 by Örjan Sandred, IRCAM 1999
; Update 1.3 augusti 2002
; Update 1.31 september 2002
;
;
; The following functions has been changed in this update:
;     lock-one-predefined-layer, predefine-rlayer-in-builddomain, RC::preset-layer
;     big-domain, RC::make-rhythm-domain
;

(in-package RC)
;-------------------------------
;ENCODEING
;function to create a rhythm domain list
(defun create-rdomain (voice-nr layer-nr rhythmcell-list)
  (loop for x from 0 to (1- (length rhythmcell-list)) 
    collect
    (let ((this-instance (make-instance 'rhythmcell))
          (rhythm-cell (nth x rhythmcell-list)))
      (set-voice-nr voice-nr this-instance)
      (set-layer-nr layer-nr this-instance)
      (set-rhythmcell rhythm-cell this-instance)
      (set-variabledur (apply '+ (mapcar 'abs rhythm-cell)) this-instance)
      (set-local-onset (om::dx->x 0 (mapcar 'abs rhythm-cell)) this-instance)
      (set-pauses (mapcar #'(lambda (x) (if (< x 0) -1 1)) rhythm-cell) this-instance)
    this-instance)))


;function to create a timesignature domain list
(defun create-mdomain (voice-nr layer-nr timesign-list)
  (loop for x from 0 to (1- (length timesign-list))
    collect
    (let ((this-instance (make-instance 'timesign))
          (timesign (nth x timesign-list)))
      (if (test-if-timesign timesign)
        (progn
          (set-voice-nr voice-nr this-instance)
          (set-layer-nr layer-nr this-instance)
          (set-timesign timesign this-instance)
          (set-variabledur (apply '/ timesign) this-instance))
        (progn (print "WARNING: The input to the domain input 0 is not time signatures!!   ")
               (om::om-beep)))
    this-instance)))


;Test that the user input a timesignature and not a rhythm)
(defun test-if-timesign (timesign)
  (and
   (eval (append '(and) 
                 (mapcar #'(lambda (x) (and (typep x 'integer)(> x 0))) timesign)))
   (= (length timesign) 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-------------------LOCK LAYERS 1

(defun set-layer-predefined-flag (voice layer-nr)
  ;Lock the layer by giving pointer to end of layer already 
  ;at index 0 (this is not possible for the Csolver to do).
  (setf (aref pointers-vector voice layer-nr 1 0) 
        (aref pointers-vector voice layer-nr 1 1)))

(defun unset-layer-predefined-flag (voice layer-nr)
  ;Make sure pointer to end of layer is zero at index 0.
  ;Predefined layer sets this value to the end point of a layer.
  ;Csolver will then think it has filled all the values until 
  ;the end point.
  (setf (aref pointers-vector voice layer-nr 1 0) 0))


(defun lock-one-predefined-layer (rhythm-seq-abs pauseflags voice layer-nr)
  ;Unlock old predefined layer, if any.
  (unset-layer-predefined-flag voice layer-nr)
  

  ;Put a very high last value to make sure the layer "never ends"
  ;Otherwise the search might stop at the end point, and no solution 
  ;will be found.
  (setf rhythm-seq-abs (append rhythm-seq-abs '(999999)))
  (setf pauseflags (append pauseflags '(0)))

  ;Put the seq in the layer
  (put-this-rhythmcell rhythm-seq-abs pauseflags voice layer-nr 0)

  ;Lock the layer
  (set-layer-predefined-flag voice layer-nr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Interface - how to lock a layer
;This function outputs a function to be sent to create-domain-and-fixlayers
;It will lock the layer it will be sent to. The locked later will be rhythm-seq-abs

(defun predefine-rlayer-in-builddomain (rhythm-seq-abs pauseflags)
#'(lambda (voice layer-nr)
    (if (= layer-nr 0) 
      (progn (print "Rhythms can't be predefined in a measure layer.")
             (om::om-beep))
      (lock-one-predefined-layer rhythm-seq-abs pauseflags voice layer-nr)))) ;OBS voice in lambda function


;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-flag-no-measure-domain (voice)
  (setf (aref part-timegrid-vector voice 1 0) 0))

(defun set-flag-measure-domain (voice)
  (setf (aref part-timegrid-vector voice 1 0) '(4 4)))

(defun check-if-measure-domain? (voice)
  (typep (aref part-timegrid-vector voice 1 0) 'list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;How to lock a measure layer

(defun create-local-grid (allowed-subdivisions timesign)
  (let* ((no-of-beats (car timesign))
         (beatvalue (cadr timesign))
         (beat-list (mapcar #'(lambda (x) (om::dx->x 0 (make-list (* no-of-beats x) :initial-element (/ 1 beatvalue x))))
                             allowed-subdivisions))
         (grid-abstime (remove-duplicates (sort (apply 'append beat-list) '<)))
         (grid (om::x->dx grid-abstime)))
    grid))



(defun build-global-grid (timesign-list beatvalue1 allowed-subdivisions1 
                                        &optional (beatvalue2 0) allowed-subdivisions2
                                        (beatvalue3 0) allowed-subdivisions3
                                        (beatvalue4 0) allowed-subdivisions4
                                        (beatvalue5 0) allowed-subdivisions5)
  (let ((global-grid nil))
    
    (dolist (timesign timesign-list)
      (cond ((= (cadr timesign) beatvalue1)
             (setf global-grid (append global-grid (create-local-grid allowed-subdivisions1 timesign)))
             )
            ((= (cadr timesign) beatvalue2)
             (setf global-grid (append global-grid (create-local-grid allowed-subdivisions2 timesign)))
             )
            ((= (cadr timesign) beatvalue3)
             (setf global-grid (append global-grid (create-local-grid allowed-subdivisions3 timesign)))
             )
            ((= (cadr timesign) beatvalue4)
             (setf global-grid (append global-grid (create-local-grid allowed-subdivisions4 timesign)))
             )
            ((= (cadr timesign) beatvalue5)
             (setf global-grid (append global-grid (create-local-grid allowed-subdivisions5 timesign)))
             )
            (t (setf global-grid (append global-grid (create-local-grid '(1) timesign))))
            ;This last one is a default if beatvalue is not given to the function. Like this, global
            ;time will always be correct.
            ))
    (om::dx->x 0 global-grid)))




(defun put-precalc-timesign (voice timesign-list beatvalue1 allowed-subdivisions1 
                                   &optional (beatvalue2 0) allowed-subdivisions2
                                   (beatvalue3 0) allowed-subdivisions3
                                   (beatvalue4 0) allowed-subdivisions4
                                   (beatvalue5 0) allowed-subdivisions5)

  (let ((global-grid
         (build-global-grid timesign-list beatvalue1 allowed-subdivisions1 beatvalue2 allowed-subdivisions2
                            beatvalue3 allowed-subdivisions3 beatvalue4 allowed-subdivisions4
                            beatvalue5 allowed-subdivisions5)))

  ;Put the list in vector. "Trick" the function, put the whole list but only
  ;first time signature.
  (put-this-timesign (car timesign-list) global-grid voice 0 0)
  
  ;And now you have to put all time sign as a list in the vector (the grid
  ;is already there, but not the timesign). This differs from the normal way the
  ;time signatures are put by the rule r-beat-subdiv. Here you put them next to
  ;each other in the vector. You can't access them with a pointer from pointers-vector
  ;(since this makes no sense in a pre calculated layer). Put a NIL after the list to
  ;make possible to find the end.

  (loop for pointer from 0 to (length timesign-list)
        do (setf (aref part-timegrid-vector voice 1 pointer) (nth pointer (append timesign-list nil))))
    
  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-------------------LOCK LAYER 2

(defun lock-one-measurelayer (timesign-list voice layer-nr beatvalue1 allowed-subdivisions1 
                                        &optional (beatvalue2 0) allowed-subdivisions2
                                        (beatvalue3 0) allowed-subdivisions3
                                        (beatvalue4 0) allowed-subdivisions4
                                        (beatvalue5 0) allowed-subdivisions5)
  ;Unlock old fixed layer, if any.
  (unset-layer-predefined-flag voice layer-nr)

  ;Expand layer by repeating the last time signatures over and over again.
  ;This is to avoid the search to stop at the end point (no solution 
  ;would be found).
  (setf timesign-list (append timesign-list (make-list 20 :initial-element (car (last timesign-list)))))

  ;Put the seq in the layer
  (put-precalc-timesign voice timesign-list beatvalue1 allowed-subdivisions1 
                                   beatvalue2 allowed-subdivisions2
                                   beatvalue3 allowed-subdivisions3
                                   beatvalue4 allowed-subdivisions4
                                   beatvalue5 allowed-subdivisions5)
  ;(put-this-rhythmcell rhythm-seq-abs voice layer-nr 0)

  ;Lock the layer
  (set-layer-predefined-flag voice layer-nr)
  )


(defun predefine-mlayer-in-builddomain (timesign-list beatvalue1 allowed-subdivisions1 
                                                  &optional (beatvalue2 0) allowed-subdivisions2
                                                  (beatvalue3 0) allowed-subdivisions3
                                                  (beatvalue4 0) allowed-subdivisions4
                                                  (beatvalue5 0) allowed-subdivisions5)

    #'(lambda (voice layer-nr)
        (if (/= layer-nr 0) (progn (print "timesignatures can't be predefined in a rhythm layer.")
                                   (om::om-beep))
            (lock-one-measurelayer timesign-list voice layer-nr beatvalue1 allowed-subdivisions1 
                                   beatvalue2 allowed-subdivisions2
                                   beatvalue3 allowed-subdivisions3
                                   beatvalue4 allowed-subdivisions4
                                   beatvalue5 allowed-subdivisions5))))


;nill in input list below is ok. Append will automatically ignore these.
(defun one-voice-domain (voice-nr layer-nr0 layer-nr1 layer-nr2 layer-nr3 layer-nr4
                                timesign-list rhythmcell-list1 rhythmcell-list2 rhythmcell-list3 rhythmcell-list4)
  (append (create-mdomain voice-nr layer-nr0 timesign-list)
          (create-rdomain voice-nr layer-nr1 rhythmcell-list1)
          (create-rdomain voice-nr layer-nr2 rhythmcell-list2)
          (create-rdomain voice-nr layer-nr3 rhythmcell-list3)
          (create-rdomain voice-nr layer-nr4 rhythmcell-list4)))

(defun one-voice-domain-and-predefined-layers (voice-nr input0 input1 input2 input3 input4)
  (let ((timesign-list nil) 
        (rhythmcell-list1 nil)
        (rhythmcell-list2 nil)
        (rhythmcell-list3 nil)
        (rhythmcell-list4 nil))

    (loop for layer-nr from 0 to *max-numberof-layers*
          do (unset-layer-predefined-flag voice-nr layer-nr))
    (if (typep input0 'function)
      (progn (funcall input0 voice-nr 0))
      (if input0
        (progn (setf timesign-list input0)
               (set-flag-measure-domain voice-nr))
        (set-flag-no-measure-domain voice-nr))) ;0 = voice 0
    (if (typep input1 'function)
      (funcall input1 voice-nr 1)
      (if input1
        (setf rhythmcell-list1 input1)))
    (if (typep input2 'function)
      (funcall input2 voice-nr 2)
      (if input2
        (setf rhythmcell-list2 input2)))
    (if (typep input3 'function)
      (funcall input3 voice-nr 3)
      (if input3
        (setf rhythmcell-list3 input3)))
    (if (typep input4 'function)
      (funcall input4 voice-nr 4)
      (if input4
        (setf rhythmcell-list4 input4)))
    (one-voice-domain voice-nr 0 1 2 3 4
                      timesign-list 
                      rhythmcell-list1 rhythmcell-list2 
                      rhythmcell-list3 rhythmcell-list4)

    ))

(defun set-one-voice-domain-in-next-box (mlayer rlayer1 rlayer2 rlayer3 rlayer4)
       #'(lambda (voice-nr)
           (one-voice-domain-and-predefined-layers voice-nr mlayer rlayer1 rlayer2 rlayer3 rlayer4)))



(defun create-domain-for-all-voices-pmc (voice0 &optional voice1 voice2 voice3 voice4 voice5 voice6)
  (let (domain0 domain1 domain2 domain3 domain4 domain5 domain6)
    (if voice0 
      (setf domain0 (funcall voice0 0)))
    (if voice1 
      (setf domain1 (funcall voice1 1)))
    (if voice2 
      (setf domain2 (funcall voice2 2)))
    (if voice3 
      (setf domain3 (funcall voice3 3)))
    (if voice4 
      (setf domain4 (funcall voice4 4)))
    (if voice5 
      (setf domain5 (funcall voice5 5)))
    (if voice6 
      (setf domain6 (funcall voice6 6)))
    (append domain0 domain1 domain2 domain3 domain4 domain5 domain6)))



;*********************
;om function

(om::defmethod! RC::voice-domain ((timesign-list t)
                                  (rhythmcell-list1 t)
                                  &optional rhythmcell-list2 
                                  rhythmcell-list3 
                                  rhythmcell-list4)

  :initvals '(nil nil nil nil nil nil)
  :indoc '("timesigns" "layer1" "layer2" "layer3" "layer4")
  :doc "Build a domain for one voice.

<timesign-list> is a list of possible time signature.
<rhythmcell-list1> (can be expanded up to rhythmcell-list4) is a list 
of possible rhythm cells that can exist in one layer.

If an input is set to nil, this layer is not used (for the first 
input this means that the solution will not be divided into bars. 
4//4 will be used in notation windows). A layer can also be predefined; 
i.e. the layer can not be changed by the search engine (but will 
affect other layers in the same way as for \“ordinary\” domains). 
This is done by connecting the \“preset-layer\”/\”preset-timesign\” 
boxes to the input for one layer.
--------------------------
Bygg en domän för en stämma. 

<timesign-list> är en lista av möjliga taktarter som kan förekomma.
<rhythmcell-list1> (kan expanderas upp till rhythmcell-list4) är en 
lista av möjliga rytmceller som kan förekomma i ett skikt.

Om en ingång ges värdet nil används inte det skiktet (för taktarts-
skiktet innebär det att lösningen inte indelas i taktarter. 4//4-takt 
används i notationsfönster). Ett skikt kan också fördefinieras, 
d.v.s. skiktet kan inte förändras av sökmotorn (men påverka 
andra skikt på samma sätt som \“vanliga\” domäner). Detta görs genom 
att ansluta \“preset-layer\”/\”preset-timesign\” till ingången för ett 
skikt.
"

  :icon 353

  (set-one-voice-domain-in-next-box timesign-list rhythmcell-list1 
                                    rhythmcell-list2 rhythmcell-list3 rhythmcell-list4))



(om::defmethod! RC::preset-layer  ((rhythmseq list))


  :initvals '((1/4 1/4  1/4  1/4  1/4 1/4  1/4  1/4))
  :indoc '("measurelayer" "rhythmlayer1" "rhythmlayer2" "rhythmlayer3" "rhythmlayer4")
  :doc "Predefine a rhythm layer.

<rhythmseq> is a list of duration values (ratios). Negative values 
indicate pauses.

The layer freezes to the rhythm sequence and can not be changed 
by the search engine. The output should be connected to the input 
on the \“voice-domain\” box for the layer you want to predefine.
--------------------------
Fördefiniera ett rytmskikt.

<rhythmseq> är en lista med notvärden (bråk). Negativa värden 
indikerar paus.

Skiktet fryses till rytmsekvensen och kan inte förändras av 
sökmotorn. Utgången ska anslutas till ingången på \“voice-domain\”
för det skikt man vill fördefiniera.
"

  :icon 357

  (predefine-rlayer-in-builddomain (om::dx->x 0 (om::om-abs rhythmseq)) (mapcar #'(lambda (duration) (if (< duration 0) -1 1)) rhythmseq))
  )


(om::defmethod! RC::preset-timesign  ((timesign-list list) 
                                       (beatvalue1 integer)
                                       (subdiv1 list)
                                       &optional (beatvalue2 0) subdiv2 (beatvalue3 0)  subdiv3 
                                       (beatvalue4 0)  subdiv4 (beatvalue5 0)  subdiv5)


  :initvals '('((4 4)(4 4)) 4 '(1 2 3 4 5) 8 '(1 2 3) 2 '(1 2 3) 16 '(1 2 3) 32 '(1 2 3))
  :indoc '("measurelayer" "beat" "subdivlist" "beat" "subdivlist" "beat" "subdivlist" 
           "beat" "subdivlist" "beat" "subdivlist" )
  :doc "Predefine a time signature layer.

<timesign-list> is a list of time signatures.
<beatvalue1> and <subdiv1> come in a pair (can be expanded up 
to maximum 5 pairs), and the rule \“r-beat-subdiv\” depends on them. 
<subdiv1> is a list of possible subdivisions of the beat. <beatvalue1>  
is the note value in the time signature the subdivision refers to. 
Example: If <beatvalue1> = 4, then <subdiv1> tells how the beats in a 
measure with a time signature with the beat value a quarter note can 
be subdivided. If <subdiv1> = (1 2 3 4 5) this means that the beat can 
be subdivided up to a quintuplet, however only if this is on the 
beat (i.e. not as a syncopation). See further  \“r-beat-subdiv\”.

The time signatures in the solution freeze to the given sequence of 
time signature (they can not be changed by the search engine). The 
output of the box should be connected to the first input on the 
\“voice-domain\” box for the voice you want to affect.
--------------------------
Fördefiniera ett taktartsskikt.

<timesign-list> är en lista med taktarter.
<beatvalue1> och <subdiv1> hör ihop i par (kan expanderas upp 
till maximalt 5 par), och är till för att regeln \“r-beat-subdiv\” 
ska kunna fungera. <subdiv1> är en lista av möjliga underdelningar 
av ett pulsslag. <beatvalue1> är notvärdet i den taktarstsignatur 
underdelningen avser. Exempel: Om <beatvalue1> = 4 anger <subdiv1> hur 
pulsslagen en taktart med pulsvärdet en fjärdedelsnot kan underdelas. 
Om <subdiv1> = (1 2 3 4 5) betyder det att pulsslaget kan underdelas 
upp till kvintol, dock endast om denna hamnar på slaget (d.v.s. 
inte som synkop). Se vidare \“r-beat-subdiv\”.


Taktarterna för lösningen fryses till den givna sekvensen av 
taktarter (de kan inte förändras av sökmotorn). Utgången ska 
anslutas till första ingången på \“voice-domain\” för den stämma
man vill påverka.
"
  :icon 357

  (predefine-mlayer-in-builddomain timesign-list beatvalue1 subdiv1 
                               beatvalue2 subdiv2
                               beatvalue3 subdiv3
                               beatvalue4 subdiv4
                               beatvalue5 subdiv5))

(om::defmethod! RC::domains->pmc ((n-var integer)
                                  (voice0 t)
                                  &optional voice1 voice2
                                  voice3 voice4 
                                  voice5 voice6)
  
  :initvals '(1 nil nil nil nil nil nil)
  :indoc '("number of variables" "voice-domain" "voice-domain" "voice-domain" "voice-domain" "voice-domain" "voice-domain")
  :doc "Format all voice-domains for the pmc <s-space> input.

<n-var> is the number of variables that are asked for in the solution.
<voice0> (can be expanded up to voice6) should be connected to a 
\“voice-domain\” output. A \“voice-domain\” is identified by which 
entrance it is connected to. Only rules that are connected to the 
corresponding input on the \“rules->pmc\” will affect the domain.
--------------------------
Formatera alla domäner för ingången <s-space> på pmc.

<n-var> är antalet variabler som önskas i lösningen.
<voice0> (kan expanderas upp till voice6) ska anslutas till utgången från en 
\“voice-domain\”. En \“voice-domain\” identifieras utifrån vilken av dessa 
ingångar den är ansluten till. Endast regler som är anslutna till motsvarande 
ingång på \“rules->pmc\” kommer att påverka domänen.
"
  :icon 375
    
  (make-list n-var :initial-element
             (create-domain-for-all-voices-pmc voice0 voice1 voice2 voice3 voice4 voice5 voice6)))


(om::defmethod! RC::rhythmdomain->voices ((domain list))
   :initvals '(nil)
   :indoc '("list of rhythmcells")
   :doc "Convert a domain (or any list of rhythm cells) to a list of voices.

<domain> is a list of rhythm cells, notated as lists of ratios.

4//4 will be used as meter. The longest cell decides the length 
for the notation.
--------------------------
Konvertera en domän (eller vilken lista av rytmceller som helst) 
till en lista av voice-objekt.

<domain> är en lista av rytmceller, noetrade som listor av bråk.

4//4 kommer att användas som taktart. Den längsta cellen avgör 
längden för notationen.
"
   :icon 387
   
   (let ((nr-of-measures (apply 'max (loop for n from 0 to (1- (length domain))
                                           collect (apply '+ (mapcar 'abs (nth n domain)))))))
     
     (if (/= 0 (rem nr-of-measures 1))
       (setf nr-of-measures (1+ (truncate nr-of-measures))))
     
     (loop for n from 0 to (1- (length domain))
           collect (make-instance 'om::voice 
                     :tree (simpleformat->tree (nth n domain) (make-list nr-of-measures :initial-element '(4 4)))
                     :legato 99)
           )
     
     ))


;******CSOLVER COMPATIBILITY

(defun create-domain-for-all-voices-csolv (voice0 &optional voice1 voice2 voice3 voice4 voice5 voice6)
  (let (domain0 domain1 domain2 domain3 domain4 domain5 domain6)
    (if voice0 
      (setf domain0 (funcall voice0 0)))
    (if voice1 
      (setf domain1 (funcall voice1 1)))
    (if voice2 
      (setf domain2 (funcall voice2 2)))
    (if voice3 
      (setf domain3 (funcall voice3 3)))
    (if voice4 
      (setf domain4 (funcall voice4 4)))
    (if voice5 
      (setf domain5 (funcall voice5 5)))
    (if voice6 
      (setf domain6 (funcall voice6 6)))
    (om::variable-domains (append domain0 domain1 domain2 domain3 domain4 domain5 domain6))))



(om::defmethod! RC::domains->Csolver ((voice0 t)
                                      &optional voice1 voice2
                                      voice3 voice4 
                                      voice5 voice6)
  
   :initvals '(nil nil nil nil nil nil)
    :indoc '("voice-domain" "voice-domain" "voice-domain" "voice-domain" "voice-domain" "voice-domain")
    :doc "Format all voice-domains for the Csolver <data> input.

<voice0> (can be expanded up to voice6) should be connected to a 
\“voice-domain\” output. A \“voice-domain\” is identified by which 
entrance it is connected to. Only rules that are connected to the 
corresponding input on the \“rules->csolver\” will affect the domain.

The number of variables in the solution is defined on the Csolver (<n-ch>).
--------------------------
Formatera alla domäner för ingången <data> på Csolver.

<voice0> (kan expanderas upp till voice6) ska anslutas till utgången från en 
\“voice-domain\”. En \“voice-domain\” identifieras utifrån vilken av dessa 
ingångar den är ansluten till. Endast regler som är anslutna till motsvarande 
ingång på \“rules->csolver\” kommer att påverka domänen.

Antalet variablar i lösningen definieras på Csolver (<n-ch>).
"
    :icon 365
    
    (create-domain-for-all-voices-csolv voice0 voice1 voice2 voice3 voice4 voice5 voice6))


;****BACK COMPATIBILITY

;(om::defmethod! RC::lock-layer  ((rhythmseq list) 
;                                (layer-nr integer)) 
;  :initvals '('(0) 3)
;  :indoc '("rhythmseq" "layer-nr")
;  :doc "pre-fix one layer"
;  :icon 353
;  
;  (lock-one-predefined-layer rhythmseq 0 layer-nr)
;  )

;(om::defmethod! RC::unlock-layer ((layer-nr integer))
;  :initvals '(3)
;  :indoc '("layer-nr")
;  :doc "unlock a fixed layer"
;  :icon 353
;  
;  (unset-layer-predefined-flag 0 layer-nr)
;  )


;;;NEW function 1.31

(defun big-domain (shortest-dur longest-dur tuplets pauses)
  (let ((durations (om::arithm-ser shortest-dur longest-dur shortest-dur)))
    
    (if tuplets
      (if (typep tuplets 'integer)
        (setf durations (append durations (om::arithm-ser (/ (* 2 shortest-dur) tuplets) longest-dur (/ (* 2 shortest-dur) tuplets))))
        (if (typep tuplets 'list)
          (setf durations (append durations
                                  (mapcar #'(lambda (tuplet) (om::arithm-ser (/ (* 2 shortest-dur) tuplet) longest-dur (/ (* 2 shortest-dur) tuplet)))
                                          tuplets)))
          (print "error in tuplets to big-domain"))))
    (if pauses 
      (mapcar 'list (append (remove-duplicates (sort (om::flat durations) '<))
                            (om::om- 0 (remove-duplicates (sort (om::flat durations) '<)))))
      (mapcar 'list (remove-duplicates (sort (om::flat durations) '<))))))



(om::defmethod! RC::make-rhythm-domain ((shortest-dur number)
                                        (longest-dur number)
                                        &optional (tuplets nil)
                                        (pauses nil))
   
   :initvals '(1/16 1 nil nil )
   :indoc '("duration" "duration" "subdiv" "?")
   :menuins '((3 (("yes" 't) ("no" 'nil) )))
   :doc ""
   :icon 389
   
   (big-domain shortest-dur longest-dur tuplets pauses))
