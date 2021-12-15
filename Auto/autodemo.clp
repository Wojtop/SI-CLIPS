
;;;======================================================
;;;   Automotive Expert System
;;;
;;;     This expert system diagnoses some simple
;;;     problems with a car.
;;;
;;;     CLIPS Version 6.3 Example
;;;
;;;     For use with the Auto Demo Example
;;;======================================================

;;; ***************************
;;; * DEFTEMPLATES & DEFFACTS *
;;; ***************************

(deftemplate UI-state
   (slot id (default-dynamic (gensym*)))
   (slot display)
   (slot relation-asserted (default none))
   (slot response (default none))
   (multislot valid-answers)
   (slot state (default middle)))
   
(deftemplate state-list
   (slot current)
   (multislot sequence))
  
(deffacts startup
   (state-list))
   
;;;****************
;;;* STARTUP RULE *
;;;****************

(defrule system-banner ""

  =>
  
  (assert (UI-state (display WelcomeMessage)
                    (relation-asserted start)
                    (state initial)
                    (valid-answers))))

;;;***************
;;;* QUERY RULES *
;;;***************

(defrule first-question
   (logical (start))

   =>

   (assert (UI-state (display FirstQuestion)
                     (relation-asserted first-choice)
                     (response UHPQ)
                     (valid-answers UHPQ Modd EoU BBV Port))))  

;;;**************************
;;;* Powtarzajace sie stany *
;;;**************************

;;; first-choice UHPQ/Modd/EoU/BBV/Port
;;; affaffordability Yes/No
;;; dual-extrusion Yes/No
;;; makerBot Yes/No
;;; DIY-ass Kit/Ass
;;; big-build Yes/No
;;; really-big-build Big/RBig

;;; w ponizszych regulach modyfikować warunki logiczne przez dodawanie OR'ow

;; affordability
(defrule affordability-question ""

   (logical (first-choice ~Port))

   =>

   (assert (UI-state (display Affordability)
                     (relation-asserted affordability)
                     (response No)
                     (valid-answers No Yes))))

;; dual extursion
(defrule dual-extrusion-question ""

   (logical (affordability Yes)
            (first-choice UHPQ))

   =>

   (assert (UI-state (display DualExtrusionQuest)
                     (relation-asserted dual-extrusion)
                     (response No)
                     (valid-answers No Yes))))   

;; makerBot 
(defrule makerBot-question ""
   (or
      (logical (first-choice Port))
      (and 
            (logical (first-choice BBV)
            (affordability No)
            ) 
      )
   )
   =>

   (assert (UI-state (display OposedToMakerBot)
                     (relation-asserted makerBot)
                     (response No)
                     (valid-answers No Yes))))                     

;; DIY vs assembled
(defrule DIY-assembled-question""
   (logical 
      (first-choice BBV)
      (affordability Yes)
      (really-big-build RBig)
   )
   =>

   (assert (UI-state (display DIYvsAss)
                     (relation-asserted DIY-ass)
                     (response Kit)
                     (valid-answers Kit Ass))))   

;; big build volume
; (defrule big-build-question ""

   
   

;    =>

;    (assert (UI-state (display BigBuildVolume)
;                      (relation-asserted big-build)
;                      (response No)
;                      (valid-answers No Yes))))  

;; really big build volume
(defrule really-big-build-question ""
   (logical 
      (first-choice BBV)
      (affordability Yes)
   )
   =>

   (assert (UI-state (display ReallyBigBuildVolume)
                     (relation-asserted really-big-build)
                     (response Big)
                     (valid-answers RBig Big)))) 

;;;********************************
;;;* Koniec powtarzajacych stanow *
;;;********************************



(defrule UHPQ-no-affordability ""

   (logical (affordability No)
            (first-choice UHPQ))

   =>

      (assert (UI-state (display Ultimaker2)
                     (state final))))                     
                 

(defrule UHQP-dual-extrusion-yes ""

   (logical (dual-extrusion Yes)
            (affordability Yes)
            (first-choice UHPQ))

   =>

      (assert (UI-state (display DeezmakerBukbotV2Duo)
                     (state final))))
                     
                     
                     
(defrule UHPQ-dual-extrusion-no ""

   (logical (dual-extrusion No)
            (affordability Yes)
            (first-choice UHPQ))

   =>

      (assert (UI-state (display DeezmakerBukbotV2)
                     (state final))))                     
   
               

(defrule Port-no-makerBot ""

   (logical (first-choice Port)
            (makerBot No))

   =>

   (assert (UI-state (display MkBotRepMini)
                     (state final))))      

(defrule portability-level-question ""

   (logical (first-choice Port)
            (makerBot Yes))

   =>

   (assert (UI-state (display PortabilityLvl)
                     (relation-asserted portability-lvl)
                     (response FaBO)
                     (valid-answers FaBO LaS))))    


(defrule portability-level-battery ""

   (logical (first-choice Port)
            (makerBot Yes)
            (portability-lvl FaBO))

   =>

        (assert (UI-state (display PortabeeGo)
                     (state final))))   

(defrule portability-vs-materials-question ""

   (logical (first-choice Port)
            (makerBot Yes)
            (portability-lvl LaS))

   =>

   (assert (UI-state (display PortVsMat)
                     (relation-asserted port-vs-materials)
                     (response MoreMat)
                     (valid-answers MoreMat Portability))))                                

(defrule portability-final-materials ""

   (logical (port-vs-materials MoreMat))
   =>
        (assert (UI-state (display DeezMakerBukito)
                     (state final)))) 

(defrule portability-final-portability ""

   (logical (port-vs-materials Portability))
   =>
        (assert (UI-state (display PrintrbotSimpleMetal)
                     (state final))))  

(defrule big-build-no-affordability-no-makerBot ""

   (logical (first-choice BBV)
      (affordability No)
      (makerBot Yes)
   )

   =>
        (assert (UI-state (display PrintLeCrXL)
                     (state final))))  

(defrule big-build-no-affordability-makerBot ""
   (logical (first-choice BBV)
         (affordability No)
         (makerBot No)
      )

      =>
         (assert (UI-state (display PrintMkrRepZ18)
                        (state final)))
)

(defrule big-build-affordability-bigV ""
   (logical (first-choice BBV)
         (affordability Yes)
         (really-big-build Big)
      )

      =>
         (assert (UI-state (display RigidbotBig)
                        (state final)))
)

(defrule big-build-affordability-really-big-assembled ""
   (logical (first-choice BBV)
         (affordability Yes)
         (really-big-build RBig)
         (DIY-ass Ass)
      )

      =>
         (assert (UI-state (display WanhaoDuplicator5)
                        (state final)))
)
(defrule big-build-affordability-really-big-kit ""
   (logical (first-choice BBV)
         (affordability Yes)
         (really-big-build RBig)
         (DIY-ass Kit)
      )

      =>
         (assert (UI-state (display SMCRMv2)
                        (state final)))
)

                     
;;;*************************
;;;* GUI INTERACTION RULES *
;;;*************************

(defrule ask-question

   (declare (salience 5))
   
   (UI-state (id ?id))
   
   ?f <- (state-list (sequence $?s&:(not (member$ ?id ?s))))
             
   =>
   
   (modify ?f (current ?id)
              (sequence ?id ?s))
   
   (halt))

(defrule handle-next-no-change-none-middle-of-chain

   (declare (salience 10))
   
   ?f1 <- (next ?id)

   ?f2 <- (state-list (current ?id) (sequence $? ?nid ?id $?))
                      
   =>
      
   (retract ?f1)
   
   (modify ?f2 (current ?nid))
   
   (halt))

(defrule handle-next-response-none-end-of-chain

   (declare (salience 10))
   
   ?f <- (next ?id)

   (state-list (sequence ?id $?))
   
   (UI-state (id ?id)
             (relation-asserted ?relation))
                   
   =>
      
   (retract ?f)

   (assert (add-response ?id)))   

(defrule handle-next-no-change-middle-of-chain

   (declare (salience 10))
   
   ?f1 <- (next ?id ?response)

   ?f2 <- (state-list (current ?id) (sequence $? ?nid ?id $?))
     
   (UI-state (id ?id) (response ?response))
   
   =>
      
   (retract ?f1)
   
   (modify ?f2 (current ?nid))
   
   (halt))

(defrule handle-next-change-middle-of-chain

   (declare (salience 10))
   
   (next ?id ?response)

   ?f1 <- (state-list (current ?id) (sequence ?nid $?b ?id $?e))
     
   (UI-state (id ?id) (response ~?response))
   
   ?f2 <- (UI-state (id ?nid))
   
   =>
         
   (modify ?f1 (sequence ?b ?id ?e))
   
   (retract ?f2))
   
(defrule handle-next-response-end-of-chain

   (declare (salience 10))
   
   ?f1 <- (next ?id ?response)
   
   (state-list (sequence ?id $?))
   
   ?f2 <- (UI-state (id ?id)
                    (response ?expected)
                    (relation-asserted ?relation))
                
   =>
      
   (retract ?f1)

   (if (neq ?response ?expected)
      then
      (modify ?f2 (response ?response)))
      
   (assert (add-response ?id ?response)))   

(defrule handle-add-response

   (declare (salience 10))
   
   (logical (UI-state (id ?id)
                      (relation-asserted ?relation)))
   
   ?f1 <- (add-response ?id ?response)
                
   =>
      
   (str-assert (str-cat "(" ?relation " " ?response ")"))
   
   (retract ?f1))   

(defrule handle-add-response-none

   (declare (salience 10))
   
   (logical (UI-state (id ?id)
                      (relation-asserted ?relation)))
   
   ?f1 <- (add-response ?id)
                
   =>
      
   (str-assert (str-cat "(" ?relation ")"))
   
   (retract ?f1))   

(defrule handle-prev

   (declare (salience 10))
      
   ?f1 <- (prev ?id)
   
   ?f2 <- (state-list (sequence $?b ?id ?p $?e))
                
   =>
   
   (retract ?f1)
   
   (modify ?f2 (current ?p))
   
   (halt))
   
