;****************************
;Rhythm Constraints library version 1.0 by Örjan Sandred, IRCAM 1999
;
;4 rhythm layers possible (limited by the class stored section)
;
;Update 1.3 18/8 2002
;To avoid errors with pausflags for timepoint 0, 1000 is added to all timepoints in the stored sections.
;
;
;The following functions below has been updated: get-rhythmlayer-within-interval, lock-layer-in-section
;    convert-stored-section->voices, convert-stored-section->poly, convert-stored-section->simple,
;    RC::r-lock-to-stored

(in-package rc)

 
;;***********************************************************************
;USES (omcs::cur-index) FROM OMCS
;The below function takes the info from the last pmc calculation.
;If search was interupted, the partial solution will be used (however
;this might not be viewed in the score!!)

(om::defclass! stored-section ()
  ((timesigns :type list :initform '() :accessor timesigns :initarg :timesigns)
   (layer1 :type list :initform '() :accessor layer1 :initarg :layer1)
   (layer2 :type list :initform '() :accessor layer2 :initarg :layer2)
   (layer3 :type list :initform '() :accessor layer3 :initarg :layer3)
   (layer4 :type list :initform '() :accessor layer4 :initarg :layer4)
   (archiveinfo :type list :initform '() :accessor archiveinfo :initarg :archiveinfo))
  (:icon 383)
  (:documentation "This is my doc"))
;The slot archive info: (startpoint-section endpoint-section offset-to-first-measure-start)

(defun get-last-solindex-pmc ()
  (if (< (omcs::cur-index) 3)
    0
    (- (omcs::cur-index) 3)))


(defun get-used-mlayer (voice last-indexx)
  (if (member 0 (detect-fix-layer voice))
    (decode-measure-fixlayers voice)
    (if (check-if-measure-domain? voice)
      (get-one-measurelayer voice 0 last-indexx)
      (make-list 1000 :initial-element '(4 4))))) ;maximum 1000 measures default

(defun used-measure-start-times (voice last-indexx)
  (if (typep (car (get-used-mlayer voice last-indexx)) 'list) 
    ; check if there are any measures (= if pmc has been evaluated), if not: output default
    (om::dx->x 0 (mapcar #'(lambda (timesign) (apply '/ timesign))
                         (get-used-mlayer voice last-indexx)))
    '(0)))

(defun get-one-measure-starttime (voice measure-nr last-index)
  (if (nth (1- measure-nr) (used-measure-start-times voice last-index)) ; 1- because first measure is called 1 (not 0)
    (nth (1- measure-nr) (used-measure-start-times voice last-index))
    0))

;get-an-interval gives the absolut time for the interval (measurenr +offset) 
;(measurenr +offset) - (measurenr +offset)
(defun get-an-interval (voice timepoint1 timepoint2 last-index)
  (let ((measure-nr1 (car timepoint1))
        (offset1 (cadr timepoint1))
        (measure-nr2 (car timepoint2))
        (offset2 (cadr timepoint2)))
    (list
     (+ (get-one-measure-starttime voice measure-nr1 last-index)
        offset1)
     (+ (get-one-measure-starttime voice measure-nr2 last-index)
        offset2))))


(defun get-rhythmlayer-within-interval (voice layer-nr timepoint1 timepoint2 last-index)
  (let* ((timeintervall (get-an-interval voice timepoint1 timepoint2 last-index))
         (startpoint (+ 1000 (car timeintervall)))                                   ;; +1000 to avoid timepoint 0
         (endpoint (+ 1000 (cadr timeintervall)))                                    ;; +1000 to avoid timepoint 0
         (rhythmseq (om::om+ 1000 (get-one-rhythmlayer voice layer-nr last-index)))  ;; +1000 to avoid timepoint 0
         (pause-flags (append (get-one-layer-pauseflags voice layer-nr last-index) '(1)))
         filtered-seq)

    ;add pauseinfo
    (setf rhythmseq (om::om* rhythmseq pause-flags))

    ;Filter out low values
    (setf filtered-seq (member startpoint rhythmseq :test #'(lambda (x y) (<= x (abs y)))))

    ;Filter out high values
    (let ((pointer-to-list 0))
      (loop until (or (>= pointer-to-list (length filtered-seq))
                      (> (nth pointer-to-list filtered-seq) endpoint))
            collect (progn (setf pointer-to-list (1+ pointer-to-list))
                           (nth (1- pointer-to-list) filtered-seq))))))


(defun calc-time-to-next-measure-start (timesign offset)
  (if timesign
    (- (apply '/ timesign) offset)
    0))


(defun get-measurelayer-within-interval (voice timepoint1 timepoint2 last-indexx)
  (let ((startmeasure (1- (car timepoint1)))  ; 1- because first measure is called 1 (not 0)
        (endmeasure (1- (car timepoint2)))
        (measureseq (get-used-mlayer voice last-indexx))
        offset
        answer)

    (if (typep (car measureseq) 'list) ; ta bort test???
      (progn (setf answer (remove nil (loop for measure-nr from startmeasure to endmeasure
                                            collect (nth measure-nr measureseq))))
             (setf offset (calc-time-to-next-measure-start (first answer)  (cadr timepoint1))))  
      (setf answer nil))

    (if (= (cadr timepoint2) 0)
      (setf answer (butlast answer))) ;take away last timesign if endpoint offset is 0

    (if (/= (cadr timepoint1) 0) ;take away first timesign if not first beat within range
      (list (cdr answer) offset)
      (list answer 0))))


(defun rhythmlayer-exist? (voice layer-nr last-index)
  (if (equal '(0) (get-one-rhythmlayer voice layer-nr last-index))
    nil
    t))

(defun store-one-section-in-archive-pmc (voice timepoint1 timepoint2)
  (let ((this-archive (make-instance 'stored-section)))
    (setf (slot-value this-archive 'timesigns) (car (get-measurelayer-within-interval voice timepoint1 timepoint2 (get-last-solindex-pmc))))
    (setf (slot-value this-archive 'layer1) (if (rhythmlayer-exist? voice 1 (get-last-solindex-pmc))
                                              (get-rhythmlayer-within-interval voice 1 timepoint1 timepoint2 
                                                                               (get-last-solindex-pmc))
                                              nil))
    (setf (slot-value this-archive 'layer2) (if (rhythmlayer-exist? voice 2 (get-last-solindex-pmc))
                                              (get-rhythmlayer-within-interval voice 2 timepoint1 timepoint2 
                                                                               (get-last-solindex-pmc))
                                              nil))
    (setf (slot-value this-archive 'layer3) (if (rhythmlayer-exist? voice 3 (get-last-solindex-pmc))
                                              (get-rhythmlayer-within-interval voice 3 timepoint1 timepoint2 
                                                                               (get-last-solindex-pmc))
                                              nil))
    (setf (slot-value this-archive 'layer4) (if (rhythmlayer-exist? voice 4 (get-last-solindex-pmc))
                                              (get-rhythmlayer-within-interval voice 4 timepoint1 timepoint2 
                                                                               (get-last-solindex-pmc))
                                              nil))
    (setf (slot-value this-archive 'archiveinfo) (append (get-an-interval voice timepoint1 timepoint2 (get-last-solindex-pmc))
                                                         (cdr (get-measurelayer-within-interval voice timepoint1 timepoint2 (get-last-solindex-pmc)))))
    this-archive))



(defun lock-layer-in-section (locked-onset-times start/stop-locked-window layernr-to-lock)
  
  (list
   #'(lambda (indexx x)
       (let ((this-layer-nr (get-layer-nr x))
             (voice (get-voice-nr x)))
         
         (if (= this-layer-nr layernr-to-lock)
           (let* ((start-this-cell (+ 1000 (get-stop-time voice this-layer-nr (1- indexx))))   ;;1000 to compensate for archived offset
                  (this-global-cell (om::om* (mapcar #'(lambda (onset) (+ start-this-cell onset)) (get-local-onset x))
                                             (append (get-pauses x) '(1))))  ;;?????
                  (windowed-this-global-cell (fast-band-filter2 (+ 1000 (car start/stop-locked-window))
                                                                (+ 1000 (cadr start/stop-locked-window))
                                                                this-global-cell))
                  (windowed-locked-onset-times (fast-band-filter2 (abs (car this-global-cell))
                                                                  (+ (get-variabledur x) start-this-cell)
                                                                  locked-onset-times)))

             ;;;special case: pause on stoptime for cell - don't check if pause, since offset don't carry pause information.
             (if (member (- (+ (get-variabledur x) start-this-cell))
                         windowed-locked-onset-times)
               
               (setf windowed-locked-onset-times (cons (abs (first windowed-locked-onset-times))
                                                      (cdr windowed-locked-onset-times))))
                                                        
             
             (equal windowed-this-global-cell windowed-locked-onset-times))
           t)))))


(defun lock-mlayer-in-section (locked-timesigns locked-section-start/stop locked-measure-offset)
  (let* ((locked-section-start (car locked-section-start/stop))
         (locked-measures-starttimes
          (om::dx->x (+ locked-section-start locked-measure-offset)
                     (mapcar #'(lambda (timesign) (apply '/ timesign))
                             locked-timesigns))))
    (list
     #'(lambda (indexx x)
         (let ((this-layer-nr (get-layer-nr x))
               (voice (get-voice-nr x)))
           
           (if (= this-layer-nr 0)
             (let ((start-this-cell (get-stop-time voice 0 (1- indexx))))
               (if (and (>= start-this-cell locked-section-start)
                        (< start-this-cell (cadr locked-section-start/stop)))
                 
                 (if (member start-this-cell locked-measures-starttimes)
                   (let ((this-locked-timesign
                          (car (remove nil (mapcar #'(lambda (starttime timesign) 
                                                       (if (= starttime start-this-cell) timesign nil))
                                                   locked-measures-starttimes
                                                   locked-timesigns)))))
                     (equal this-locked-timesign (get-timesign x)))
                   nil)
                 
                 (if (and (> start-this-cell (cadr locked-section-start/stop))   ;special case: solver jumped over section with a too large timesignature
                          (< (get-start-time voice 0 (1- indexx)) locked-section-start)
                          locked-timesigns)
                   nil
                   t)))
             t))))))



(defun lock-section  (archive-instance offset layernr0 layernr1 layernr2 layernr3 layernr4)
  
  (let ((layernrs (remove nil (list layernr0 layernr1 layernr2 layernr3 layernr4))))
    
    
    (append
     (if (member 0 layernrs)
       (let ((locked-timesigns (slot-value archive-instance 'timesigns))
             (start/stop-locked-window (om::om+ offset (butlast (slot-value archive-instance 'archiveinfo))))
             (locked-measure-offset (third (slot-value archive-instance 'archiveinfo))))
         (lock-mlayer-in-section locked-timesigns start/stop-locked-window locked-measure-offset)))
     (if (member 1 layernrs)
       (let ((locked-onset-times (om::om+ offset (slot-value archive-instance 'layer1)))
             (start/stop-locked-window (om::om+ offset (butlast (slot-value archive-instance 'archiveinfo)))))
         (lock-layer-in-section locked-onset-times start/stop-locked-window 1)))
     (if (member 2 layernrs)
       (let ((locked-onset-times (om::om+ offset (slot-value archive-instance 'layer2)))
             (start/stop-locked-window (om::om+ offset (butlast (slot-value archive-instance 'archiveinfo)))))
         (lock-layer-in-section locked-onset-times start/stop-locked-window 2)))
     (if (member 3 layernrs)
       (let ((locked-onset-times (om::om+ offset (slot-value archive-instance 'layer3)))
             (start/stop-locked-window (om::om+ offset (butlast (slot-value archive-instance 'archiveinfo)))))
         (lock-layer-in-section locked-onset-times start/stop-locked-window 3)))
     (if (member 4 layernrs)
       (let ((locked-onset-times (om::om+ offset (slot-value archive-instance 'layer4)))
             (start/stop-locked-window (om::om+ offset (butlast (slot-value archive-instance 'archiveinfo)))))
         (lock-layer-in-section locked-onset-times start/stop-locked-window 4))))))




(defun convert-stored-section->voices (stored-section)
  (let* ((section-start (car (slot-value stored-section 'archiveinfo)))
         (section-stop (cadr (slot-value stored-section 'archiveinfo)))
         (section-measure-layer (slot-value stored-section 'timesigns))
         (first-measure-offset (caddr (slot-value stored-section 'archiveinfo)))
         (all-rlayers (list (slot-value stored-section 'layer1)
                            (slot-value stored-section 'layer2)
                            (slot-value stored-section 'layer3)
                            (slot-value stored-section 'layer4)))
         layerseq-pauses
         extra-time-sign
         layerseq-abs
         layerseq-dx
         rhythm-tree
         nr-locked-layers)
    
    (if section-measure-layer
      (if (/= first-measure-offset 0)
        (progn
          (setf extra-time-sign (list (numerator first-measure-offset)
                                      (denominator first-measure-offset)))
          (setf section-measure-layer (cons extra-time-sign section-measure-layer))))
      (setf section-measure-layer (list (list (numerator (- section-stop section-start))
                                              (denominator (- section-stop section-start))))))
    
    (cond
     ((nth 3 all-rlayers) (setf nr-locked-layers 3))
     ((nth 2 all-rlayers) (setf nr-locked-layers 2))
     ((nth 1 all-rlayers) (setf nr-locked-layers 1))
     (t (setf nr-locked-layers 0)))
    
    (loop for layer-nr from 0 to nr-locked-layers
          collect (progn
                    (setf layerseq-abs (om::om- (om::om-abs (remove-duplicates (append (nth layer-nr all-rlayers)
                                                                                       (list (+ section-stop 1000)))))
                                                1000))
                    (setf layerseq-pauses (mapcar #'(lambda (value) (if (> value 0) 1 -1)) (nth layer-nr all-rlayers)))
                    (if (/= (car layerseq-abs) section-start)
                      (setf layerseq-dx (om::om* (cons (- section-start (car layerseq-abs)) (om::x->dx layerseq-abs))
                                                 (cons 1 layerseq-pauses)))
                      (setf layerseq-dx (om::om* (om::x->dx layerseq-abs) layerseq-pauses)))
                    (setf rhythm-tree (simple->tree  layerseq-dx section-measure-layer))
                    (make-instance 'om::voice 
                      :tempo 60
                      :tree rhythm-tree
                      :legato 99
                      :chords (make-list (length layerseq-dx)
                                         :initial-element
                                         (make-instance 'om::chord
                                           :LChan (list (1+ layer-nr))))
                      ))
          )))



(defun convert-stored-section->poly (stored-section)
  (make-instance 'om::poly 
    :voices
    (let* ((section-start (car (slot-value stored-section 'archiveinfo)))
           (section-stop (cadr (slot-value stored-section 'archiveinfo)))
           (section-measure-layer (slot-value stored-section 'timesigns))
           (first-measure-offset (caddr (slot-value stored-section 'archiveinfo)))
           (all-rlayers (list (slot-value stored-section 'layer1)
                              (slot-value stored-section 'layer2)
                              (slot-value stored-section 'layer3)
                              (slot-value stored-section 'layer4)))
           layerseq-pauses
           extra-time-sign
           layerseq-abs
           layerseq-dx
           rhythm-tree
           nr-locked-layers)
      
      (if section-measure-layer
        (if (/= first-measure-offset 0)
          (progn
            (setf extra-time-sign (list (numerator first-measure-offset)
                                        (denominator first-measure-offset)))
            (setf section-measure-layer (cons extra-time-sign section-measure-layer))))
        (setf section-measure-layer (list (list (numerator (- section-stop section-start))
                                                (denominator (- section-stop section-start))))))
      
      (cond
       ((nth 3 all-rlayers) (setf nr-locked-layers 3))
       ((nth 2 all-rlayers) (setf nr-locked-layers 2))
       ((nth 1 all-rlayers) (setf nr-locked-layers 1))
       (t (setf nr-locked-layers 0)))
      
      (loop for layer-nr from 0 to nr-locked-layers
            collect (progn 
                      (setf layerseq-abs (om::om- (om::om-abs (remove-duplicates (append (nth layer-nr all-rlayers)
                                                                                         (list (+ section-stop 1000)))))
                                                  1000))
                      (setf layerseq-pauses (mapcar #'(lambda (value) (if (> value 0) 1 -1)) (nth layer-nr all-rlayers)))
                      (if (/= (car layerseq-abs) section-start)
                        (setf layerseq-dx (om::om* (cons (- section-start (car layerseq-abs)) (om::x->dx layerseq-abs))
                                                   (cons 1 layerseq-pauses)))
                        (setf layerseq-dx (om::om* (om::x->dx layerseq-abs) layerseq-pauses)))
                      (setf rhythm-tree (simple->tree  layerseq-dx section-measure-layer))
                      (make-instance 'om::voice 
                        :tempo 60
                        :tree rhythm-tree
                        :legato 99
                        :chords (make-list (length layerseq-dx)
                                           :initial-element
                                           (make-instance 'om::chord
                                             :LChan (list (1+ layer-nr))))
                        )))))
  )


(defun convert-stored-section->simple (stored-section)
  (let* ((section-start (car (slot-value stored-section 'archiveinfo)))
         (section-stop (cadr (slot-value stored-section 'archiveinfo)))
         (section-measure-layer (slot-value stored-section 'timesigns))
         (first-measure-offset (caddr (slot-value stored-section 'archiveinfo)))
         (all-rlayers (list (slot-value stored-section 'layer1)
                            (slot-value stored-section 'layer2)
                            (slot-value stored-section 'layer3)
                            (slot-value stored-section 'layer4)))
         layerseq-pauses
         extra-time-sign
         layerseq-abs
         nr-locked-layers)
    
    (if section-measure-layer
      (if (/= first-measure-offset 0)
        (progn
          (setf extra-time-sign (list (numerator first-measure-offset)
                                      (denominator first-measure-offset)))
          (setf section-measure-layer (cons extra-time-sign section-measure-layer))))
      (setf section-measure-layer (list (list (numerator (- section-stop section-start))
                                              (denominator (- section-stop section-start))))))
    
    (cond
     ((nth 3 all-rlayers) (setf nr-locked-layers 3))
     ((nth 2 all-rlayers) (setf nr-locked-layers 2))
     ((nth 1 all-rlayers) (setf nr-locked-layers 1))
     (t (setf nr-locked-layers 0)))
    
    
    (list
     section-measure-layer
     (loop for layer-nr from 0 to nr-locked-layers
           collect (progn 
                     (setf layerseq-abs (om::om- (om::om-abs (remove-duplicates (append (nth layer-nr all-rlayers)
                                                                                        (list (+ section-stop 1000)))))
                                                 1000))                                          ;; - 1000 to compensate for function "store-sections" coding
                     (setf layerseq-pauses (mapcar #'(lambda (value) (if (> value 0) 1 -1)) (nth layer-nr all-rlayers)))
                     
                     (if (/= (car layerseq-abs) section-start)
                       (om::om* (cons (- section-start (car layerseq-abs)) (om::x->dx layerseq-abs))
                                (cons 1 layerseq-pauses))
                       (om::om* (om::x->dx layerseq-abs)
                                layerseq-pauses))
                     )))))


(om::defmethod! RC::store-section ((voice integer)
                                   (timepoint1 list) 
                                   (timepoint2 list)) 
  :initvals '(0 (1 0) (2 0))
  :indoc '("voicenr" "(measure offset)" "(measure offset)")
  :doc "Store a section from the last solution (pmc).

<voice> is the number of the voice from which the section will 
be taken. 
<timepoint1> and <timepoint2> are the start timepoint and the end 
timepoint in the the last solution between which the section will 
be taken. The format is a list of measure number and offset, for 
example (2 1/4) means one quarter note after the start of measure 
number 2.

The output should be connected to the rule \“r-lock-to-stored\”, or 
optional you can generate instances of the class \“stored-section\”. 
See further \“r-lock-to-stored\”. 

It is highly recommended that you always lock this box before 
evaluating it – in the new solution the bar lines might be on 
new positions, and the <timepoint1> and <timepoint2> will then 
not have the same meaning.

Do not confuse the last found solution with what might be visible 
in a notation window (the information is taken from internal 
vectors connected to the search engine – every time the search 
engine is run, the last solution will be overwritten).
--------------------------
Lagra en sektion från senaste lösningen.

<voice> är numret för den stämma från vilken sektionen kommer 
att tas.
<timepoint1> och <timepoint2> är start- och stoptidpunkter i den 
senaste lösningen mellan vilka sektionen kommer att tas. Formatet 
är en lista med taktnummer och offset, till exempel (2 1/4) betyder 
en fjärdedel efter starten på takt två.

Utgången ska kopplas till regeln \“r-lock-to-stored\”, eller så kan 
man alternativt generera instanser av klassen \“stored-section\”. 
Se vidare \“r-lock-to-stored\”. 

Det rekommenderas att alltid låsa funktionen innan man evaluerar 
den – i den nya lösningen kan taktstrecken vara på nya positioner, 
och <timepoint1> och <timepoint2> kommer då inte att ha samma 
innebörd.

Blanda inte ihop den senaste lösningen med vad som kan vara synligt 
i ett notationsfönster (informationen tas från interna vektorer 
kopplade till sökmotorn – varje gång sökmotorn körs skrivs den 
senaste lösningen över). 
"
  :icon 384
  
  (store-one-section-in-archive-pmc voice timepoint1 timepoint2))


(om::defmethod! RC::r-lock-to-stored  ((section t)
                                       (layernr0 t)
                                       &optional layernr1 layernr2 layernr3 layernr4 (offset 0))
   :initvals '(nil 1 2 3 4 0 0)
   :indoc '("from store-section" "layernr" "layernr" "layernr" "layernr" "layernr" "timepoint")
   :doc "Rule that lock a section in the solution to be identical to a stored section from an earlier solution.

<section> should either be connected to a \“store-section\” box, 
or an instance of the class \“stored-section\”.
<layernr0> (optional up to maximum layernr4) is (are) the layer 
number(s) for the layer(s) to lock.
<offset> You can optional move the stored section to a new position 
in the sequence. Offset is the distance (duration value as a ratio) 
from the start of the sequence. If offset is nil, the section will 
be locked to its original position.
--------------------------
Regel som låser en sektion i lösningen till att vara identisk med en 
lagrad sektion från en tidigare lösning.

<section> ska antingen kopplas till \“store-section\”, eller en 
instans av klassen \“stored-section\”.
<layernr0> (alternativt upp till maximalt layernr4) är numret för 
skiktet (skikten) att låsa.
<offset> Man kan alternativt flytta en sparad sektion till en ny 
position i sekvensen. Offset är avståndet (notvärde, angivet som 
ett bråk) från sekvensens start. Om offset är nil kommer sektionen 
att låsas till sin ursprungliga position.
"
   :icon 385
   
   (if (< offset -999) (progn (om-beep) 
                              (print "WARNING: Offset not less than -999 in r-lock-to-stored. This rule will be bypassed.")
                              (list (lambda (x y) (progn x y t))))
       (lock-section section offset layernr0 layernr1 layernr2 layernr3 layernr4)))



(om::defmethod! RC::decode-stored-section ((section stored-section)
                                           &optional (output 'poly))
   
   :initvals '(nil 'poly)
   :indoc '("stored-section" "format")
   :menuins '((1 (("poly" 'poly) ("voices" 'voices) ("simple" 'simple))))
   :doc "Decode the box \“store-section\” or an instance of the class \“stored-section\” to a score or to simple format.

<section> should be connected to the box \“store-section\” or an 
instance of the class \“stored-section\”.
<output> is the format for the output of this box. You can either 
get a poly-object, or a list of voice-objetcs (each one representing 
one rhythm layer), or in simple format (a list of sublists for the 
time signatures and every rhythm layer). Choose with the help of 
the pop-up menu.
--------------------------
Avkoda funktionen \“store-section\” eller en instans av klassen 
\“stored-section\” till ett partitur eller till \“simple format\”.

<section> ska antingen kopplas till \“store-section\”, eller en 
instans av klassen \“stored-section\”.
<output> är formatet för utgången. Man kan antingen få ett poly-
objekt, eller en lista med voice-objekt (där varje objekt 
representerar ett rytmskik), eller i \“simple format\” (en lista 
med sub-listor för taktartssignaturer och varje rytm skikt). 
Välj med hjälp at pop-up menyn.
"
   :icon 386
   
   (case output
     ((poly) (convert-stored-section->poly section))
     ((voices) (convert-stored-section->voices section))
     ((simple) (convert-stored-section->simple section))))



;*************CSOLVER SUPPORT


(defun get-last-solindex-csolver ()
  (if (= (om::current-variable) 0)
    0
    (1- (om::current-variable)))) ; 1- because rule does not check (and update) last index.
                                  ; This can be removed when the situation bug is fixed


(defun store-one-section-in-archive-csolver (voice timepoint1 timepoint2)
  (let ((this-archive (make-instance 'stored-section)))
    (setf (slot-value this-archive 'timesigns) (car (get-measurelayer-within-interval voice timepoint1 timepoint2)))
    (setf (slot-value this-archive 'layer1) (get-rhythmlayer-within-interval voice 1 timepoint1 timepoint2))
    (setf (slot-value this-archive 'layer2) (get-rhythmlayer-within-interval voice 2 timepoint1 timepoint2))
    (setf (slot-value this-archive 'layer3) (get-rhythmlayer-within-interval voice 3 timepoint1 timepoint2))
    (setf (slot-value this-archive 'layer4) (get-rhythmlayer-within-interval voice 4 timepoint1 timepoint2))
    (setf (slot-value this-archive 'archiveinfo) (append (get-an-interval voice timepoint1 timepoint2)
                                                         (cdr (get-measurelayer-within-interval voice timepoint1 timepoint2))))
    this-archive))

(defun store-one-section-in-archive-csolver (voice timepoint1 timepoint2)
  (let ((this-archive (make-instance 'stored-section)))
    (setf (slot-value this-archive 'timesigns) (car (get-measurelayer-within-interval voice timepoint1 timepoint2 (get-last-solindex-csolver))))
    (setf (slot-value this-archive 'layer1) (get-rhythmlayer-within-interval voice 1 timepoint1 timepoint2 (get-last-solindex-csolver)))
    (setf (slot-value this-archive 'layer2) (get-rhythmlayer-within-interval voice 2 timepoint1 timepoint2 (get-last-solindex-csolver)))
    (setf (slot-value this-archive 'layer3) (get-rhythmlayer-within-interval voice 3 timepoint1 timepoint2 (get-last-solindex-csolver)))
    (setf (slot-value this-archive 'layer4) (get-rhythmlayer-within-interval voice 4 timepoint1 timepoint2 (get-last-solindex-csolver)))
    (setf (slot-value this-archive 'archiveinfo) (append (get-an-interval voice timepoint1 timepoint2 (get-last-solindex-csolver))
                                                         (cdr (get-measurelayer-within-interval voice timepoint1 timepoint2 (get-last-solindex-csolver)))))
    this-archive))


(om::defmethod! RC::store-section-csolver ((voice integer)
                                           (timepoint1 list) 
                                           (timepoint2 list)) 
  :initvals '(0 (1 0) (2 0))
  :indoc '("voicenr" "(measure offset)" "(measure offset)")
  :doc "Store a section from the last solution (only compatible with Csolver).

<voice> is the number of the voice from which the section will 
be taken. 
<timepoint1> and <timepoint2> are the start timepoint and the end 
timepoint in the the last solution between which the section will 
be taken. The format is a list of measure number and offset, for 
example (2 1/4) means one quarter note after the start of measure 
number 2.

The output should be connected to the rule \“r-lock-to-stored\”, or 
optional you can generate instances of the class \“stored-section\”. 
See further \“r-lock-to-stored\”. 

It is highly recommended that you always lock this box before 
evaluating it – in the new solution the bar lines might be on 
new positions, and the <timepoint1> and <timepoint2> will then 
not have the same meaning.

Do not confuse the last found solution with what might be visible 
in a notation window (the information is taken from internal 
vectors connected to the search engine – every time the search 
engine is run, the last solution will be overwritten).
--------------------------
Lagra en sektion från senaste lösningen (endast kompatibel med Csolver).

<voice> är numret för den stämma från vilken sektionen kommer 
att tas.
<timepoint1> och <timepoint2> är start- och stoptidpunkter i den 
senaste lösningen mellan vilka sektionen kommer att tas. Formatet 
är en lista med taktnummer och offset, till exempel (2 1/4) betyder 
en fjärdedel efter starten på takt två.

Utgången ska kopplas till regeln \“r-lock-to-stored\”, eller så kan 
man alternativt generera instanser av klassen \“stored-section\”. 
Se vidare \“r-lock-to-stored\”. 

Det rekommenderas att alltid låsa funktionen innan man evaluerar 
den – i den nya lösningen kan taktstrecken vara på nya positioner, 
och <timepoint1> och <timepoint2> kommer då inte att ha samma 
innebörd.

Blanda inte ihop den senaste lösningen med vad som kan vara synligt 
i ett notationsfönster (informationen tas från interna vektorer 
kopplade till sökmotorn – varje gång sökmotorn körs skrivs den 
senaste lösningen över).
"
  :icon 388
  
  (store-one-section-in-archive-csolver voice timepoint1 timepoint2))

;*******BACK COMPATIBILITY

(om::defmethod! RC::stored-section->voices ((section stored-section))

   :initvals '(nil)
   :indoc '("stored-section")
   :doc "Decode a stored section to a score or simple lists."
   :icon 386
   
   (convert-stored-section->voices section))

