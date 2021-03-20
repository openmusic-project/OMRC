;;
;;
;;            Rhythm Constraints library
;;
;;            Orjan Sandred,  © IRCAM 1999 (vesrion 1.1)
;;            Orjan Sandred, © Stockholm 2002 (version 1.3)
;;
;;            update 1.3 august 2002
;;            this update adds the possibility to work with pauses in all functions/rules
;;            update 1.31 September 2002
;;            update 1.32 October 2002
;;            update 1.4 December 2004      
;;            update 1.41 January 2005
;;            update 1.42 January 2019 : merge with Ircam updates
;;            update 1.5 Sept. 2019 : merge with OM7 version
;;            
;--------------------------------------------------

(in-package om)

 
;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(mapc #'cl-user::compile&load 
      (list
       (om::om-relative-path '("sources") "package")
       (om::om-relative-path '("sources") "Classes-and-vectors")
       (om::om-relative-path '("sources") "simple2tree")
       (om::om-relative-path '("sources") "tree2simple")
       (om::om-relative-path '("sources") "Rules")
       (om::om-relative-path '("sources") "Build-domains")
       (om::om-relative-path '("sources") "decode")
       (om::om-relative-path '("sources") "measure-rules")
       (om::om-relative-path '("sources") "Access-and-lock-result")
       (om::om-relative-path '("sources") "Tools-user-rules")
       (om::om-relative-path '("sources") "Heuristic-rules")
       (om::om-relative-path '("sources") "RCUpdate")
       (om::om-relative-path '("sources") "markov-rule")
       (om::om-relative-path '("sources") "Kvantisering")
       ))


;--------------------------------------------------
; RC subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(om::fill-library 
 '(
   ("01-build domains" nil nil (rc::domains->pmc rc::voice-domain rc::preset-layer rc::preset-timesign rc::make-rhythm-domain) nil)
   ("02-decode solution" nil nil (rc::decode-engine rc::layer-in-solution rc::view-presets) nil)
   ("03a-rules interface" nil nil (rc::rules->pmc rc::heuristicrules->pmc) nil)
   ("03b-rules" (("QUANTIZE" nil nil (rc::r-quant rc::hr-quant_dev rc::hr-quant_ornaments) nil)) nil (rc::r-hierarchy rc::r-eqlength rc::r-layerorder rc::r-identical rc::r-beat-subdiv rc::r-sync-over-barline rc::r-pattern rc::r-canon rc::r-markov rc::r-order-priority rc::gr-hierarchy rc::gr-eqlength rc::gr-layerorder rc::gr-identical rc::gr-canon) nil)
   ("04-user rules tools" nil nil (rc::get-this-cell rc::get-this-cell-dur rc::get-last-cell rc::get-cell-before-last rc::get-cell-two-before-last rc::get-all-cells 
                                                 rc::get-cell-other-layer rc::get-cell-any-layer rc::get-rhythm-other-layer rc::get-rhythm-any-layer rc::get-all-cells-any-layer 
                                                 rc::get-time get-layernumber rc::get-voicenumber rc::pause? rc::rhythmcell? rc::timesign? rc::test-equal rc::test-not-equal) nil)
   ("05-lock sections" nil (rc::stored-section) (rc::store-section rc::r-lock-to-stored rc::decode-stored-section) nil)
   ("06-formating" nil nil (rc::simpleformat->tree rc::tree->simpleformat rc::rhythmdomain->voices) nil)
   ))


(om::set-lib-release 1.5)

(print "
;;;=========================================
;;; OMRC 1.5
;;; Rhythm Constraints library by O. Sandred
;;;=========================================
")