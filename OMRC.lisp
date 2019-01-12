;;
;;
;;            Rhythm Constraints library version 1.3
;;
;;            Orjan Sandred,  © IRCAM 1999 (vesrion 1.1)
;;            Orjan Sandred, © Stockholm 2002 (version 1.3)
;;
;;            update 1.3 august 2002
;;            this update adds the possibility to work with pauses in all functions/rules
;; 
;;             
;;            


;--------------------------------------------------
(defvar RC)
(defpackage RC)
(defpackage omcs)
(in-package RC)

 
;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(defvar *OM_RC-lib-files* nil)
(setf *OM_RS-lib-files* (list
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

                          ))

;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(mapc #'cl-user::compile&load *OM_RS-lib-files*)


;--------------------------------------------------
; RC subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(defvar *subpackages-list* nil)
(setf *subpackages-list*
      '(
        ("01-build domains" nil nil (domains->pmc voice-domain preset-layer preset-timesign) nil)
        ("02-decode solution" nil nil (decode-engine layer-in-solution view-presets) nil)
        ("03a-rules interface" nil nil (rules->pmc heuristicrules->pmc) nil)
        ("03b-rules" nil nil (r-hierarchy r-eqlength r-layerorder r-identical r-beat-subdiv r-sync-over-barline r-pattern r-canon r-order-priority gr-hierarchy gr-eqlength gr-layerorder gr-identical gr-canon) nil)
        ("04-user rules tools" nil nil (get-this-cell get-this-cell-dur get-last-cell get-cell-before-last get-cell-two-before-last get-all-cells 
get-cell-other-layer get-cell-any-layer get-rhythm-other-layer get-rhythm-any-layer get-all-cells-any-layer 
get-time get-layernumber get-voicenumber rhythmcell? timesign? test-equal test-not-equal) nil)
        ("05-lock sections" nil (stored-section) (store-section r-lock-to-stored decode-stored-section) nil)
        ("06-formating" nil nil (simpleformat->tree tree->simpleformat rhythmdomain->voices) nil)
        ))

;--------------------------------------------------
;filling packages
;--------------------------------------------------
(om::fill-library *subpackages-list*)
