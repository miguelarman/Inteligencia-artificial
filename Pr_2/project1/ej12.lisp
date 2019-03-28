;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nueva heurística inclyuendo heurística de coste
;;
(defparameter *estimate-new* 
  '((Calais (0.0 0.0)) (Reims (25.0 15.0)) (Paris (30.0 25.0)) 
    (Nancy (50.0 35.0)) (Orleans (55.0 63.0)) (St-Malo (65.0 95.0))
    (Nantes (75.0 110.0)) (Brest (90.0 135.0)) (Nevers (70.0 35.0))
    (Limoges (100.0 95.0)) (Roenne (85.0 40.0)) (Lyon (105.0 45.0))
    (Toulouse (130.0 130.0)) (Avignon (135.0 85.0)) (Marseille (145.0 110.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nueva definición de problema, teniendo en cuenta
;; la nueva heurística
;;
(defparameter *travel-cost-new*
  (make-problem
   :states *cities*
   :initial-state *origin*
   :f-h #'(lambda (state)
            (f-h-price state *estimate-new*))
   :f-goal-test #'(lambda (node)
                    (f-goal-test node *destination* *mandatory*))
   :f-search-state-equal #'(lambda (node-1 node-2)
                             (f-search-state-equal node-1 node-2 *mandatory*))
   :operators (list
               #'(lambda (node)
                   (navigate-train-price (node-state node) *trains* *forbidden*))
               #'(lambda (node)
                   (navigate-canal-price (node-state node) *canals*))
               )
   )
  )
