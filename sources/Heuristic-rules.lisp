;****************************
;Rhythm Constraints library version 1.0 by Ørjan Sandred, IRCAM 1999
;

(in-package RC)

(defun heuristic-rules-pmc (voice rules)
  (loop for rulex from 0 to (1- (length rules))
        collect (list '* '?1 (list 'common-lisp-user::?if 
                                   (list 'if (list '= '(get-voice-nr ?1) voice)
                                         (list 'funcall (nth rulex rules) '(1- (omcs::cur-index)) '?1)
                                         0)))))


(defun collect-heuristic-rules (rulesvoice0 rulesvoice1 rulesvoice2 
                                            rulesvoice3 rulesvoice4 
                                            rulesvoice5 rulesvoice6)  
  (apply 'append
         (if rulesvoice0 
           (heuristic-rules-pmc 0 rulesvoice0))
         (if rulesvoice1 
           (heuristic-rules-pmc 1 rulesvoice1))
         (if rulesvoice2 
           (heuristic-rules-pmc 2 rulesvoice2))
         (if rulesvoice3 
           (heuristic-rules-pmc 3 rulesvoice3))
         (if rulesvoice4 
           (heuristic-rules-pmc 4 rulesvoice4))
         (if rulesvoice5 
           (heuristic-rules-pmc 5 rulesvoice5))
         (if rulesvoice6 
           (heuristic-rules-pmc 6 rulesvoice6)))
  )


(om::defmethod! RC::heuristicrules->pmc  ((voice0 list)  &optional voice1 voice2 voice3 voice4 voice5 voice6)

  :initvals '(nil nil nil nil nil nil nil)
  :indoc '("heur.rules" "heur.rules" "heur.rules" "heur.rules" "heur.rules" "heur.rules" "heur.rules")
  :doc "Format all heuristic rules for the pmc <heuristic-rules> input.

<voice0> is a list of all heuristic rules for voice number 0 
(can be expanded up to maximum voice number 6).

A voice is identified by which entrance the box \Òvoice-domain\Ó 
is connected to on the \Òdomains->pmc\Ó box. Heuristic rules must 
be connected to the corresponding input on this box to be valid 
for the voice. See also \Òdomains->pmc\Ó.
--------------------------
Formatera alla heuristiska regler fšr ingŒngen <rules> pŒ pmc.

<voice0> Šr en lista med alla heuristiska regler fšr stŠmma 
nummer 0 (kan expanderas upp till maxiamlt stŠmma nummer 6).

En stŠmma identifieras utifrŒn vilken ingŒng \Òvoice-domain\Ó Šr 
ansluten till pŒ \Òdomains->pmc\Ó. Heuristiska regler mŒste vara 
anslutna till motsvarande ingŒng pŒ denna funktion fšr att gŠlla 
fšr en stŠmma. Se ocksŒ \Òdomains->pmc\Ó.
"
  :icon 375

  (collect-heuristic-rules voice0 voice1 voice2 voice3 voice4 voice5 voice6)
   
  )
