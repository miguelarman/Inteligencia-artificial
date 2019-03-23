;; 
;;    Lab assignment 2: Search
;;
;;    Solutions
;;    Created:  Simone Santini, 2019/03/05
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun elimina-nil (lista)
  (if (null lista)
      NIL
    (let
        ((siguiente (elimina-nil (rest lista))))
      (if (null (first lista))
          siguiente
        (cons (first lista) siguiente)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Problem definition
;;
(defstruct problem
  states               ; List of states
  initial-state        ; Initial state
  f-h                  ; reference to a function that evaluates to the 
                       ; value of the heuristic of a state
  f-goal-test          ; reference to a function that determines whether 
                       ; a state fulfils the goal 
  f-search-state-equal ; reference to a predictate that determines whether
                       ; two nodes are equal, in terms of their search state      
  operators)           ; list of operators (references to functions) to 
                       ; generate successors
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Node in search tree
;;
(defstruct node 
  state           ; state label
  parent          ; parent node
  action          ; action that generated the current node from its parent
  (depth 0)       ; depth in the search tree
  (g 0)           ; cost of the path from the initial state to this node
  (h 0)           ; value of the heurstic
  (f 0))          ; g + h 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Actions 
;;
(defstruct action
  name              ; Name of the operator that generated the action
  origin            ; State on which the action is applied
  final             ; State that results from the application of the action
  cost )            ; Cost of the action
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Search strategies 
;;
(defstruct strategy
  name              ; name of the search strategy
  node-compare-p)   ; boolean comparison
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    END: Define structures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    BEGIN: Define galaxy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *cities* '(Calais Reims Paris Nancy Orleans 
                                St-Malo Brest Nevers Limoges 
                                Roenne Lyon Toulouse Avignon Marseille))

(defparameter *trains*
  '((Paris Calais (34.0 60.0))      (Calais Paris (34.0 60.0))
    (Reims Calais (35.0 70.0))      (Calais Reims (35.0 70.0))
    (Nancy Reims (35.0 55.0))       (Reims Nancy (35.0 55.0))
    (Paris Nancy (40.0 67.0))       (Nancy Paris (40.0 67.0))
    (Paris Nevers (48.0 75.0))      (Nevers Paris (48.0 75.0))
    (Paris Orleans (23.0 38.0))     (Orleans Paris (23.0 38.0))
    (Paris St-Malo (40.0 70.0))     (St-Malo Paris (40.0 70.0))
    (St-Malo Nantes (20.0 28.0))    (Nantes St-Malo (20.0 28.0))
    (St-Malo Brest (30.0 40.0))     (Brest St-Malo (30.0 40.0))
    (Nantes Brest (35.0 50.0))      (Brest Nantes (35.0 50.0))
    (Nantes Orleans (37.0 55.0))    (Orleans Nantes (37.0 55.0))
    (Nantes Toulouse (80.0 130.0))  (Toulouse Nantes (80.0 130.0))
    (Orleans Limoges (55.0 85.0))   (Limoges Orleans (55.0 85.0))
    (Limoges Nevers (42.0 60.0))    (Nevers Limoges (42.0 60.0))
    (Limoges Toulouse (25.0 35.0))  (Toulouse Limoges (25.0 35.0))
    (Toulouse Lyon (60.0 95.0))     (Lyon Toulouse (60.0 95.0))
    (Lyon Roenne (18.0 25.0))       (Roenne Lyon  (18.0 25.0))
    (Lyon Avignon (30.0 40.0))      (Avignon Lyon (30.0 40.0))
    (Avignon Marseille (16.0 25.0)) (Marseille Avignon (16.0 25.0))
    (Marseille Toulouse (65.0 120.0)) (Toulouse Marseille (65.0 120.0)))) 
    

(defparameter *canals*
  '((Reims Calais (75.0 15.0)) (Paris Reims (90.0 10.0))
    (Paris Nancy (80.0 10.0)) (Nancy reims (70.0 20.0))
    (Lyon Nancy (150.0 20.0)) (Nevers Paris (90.0 10.0))
    (Roenne Nevers (40.0 5.0)) (Lyon Roenne (40.0 5.0))
    (Lyon Avignon (50.0 20.0)) (Avignon Marseille (35.0 10.0))
    (Nantes St-Malo (40.0 15.0)) (St-Malo Brest (65.0 15.0))
    (Nantes Brest (75.0 15.0))))



(defparameter *estimate* 
  '((Calais (0.0 0.0)) (Reims (25.0 0.0)) (Paris (30.0 0.0)) 
    (Nancy (50.0 0.0)) (Orleans (55.0 0.0)) (St-Malo (65.0 0.0))
    (Nantes (75.0 0.0)) (Brest (90.0 0.0)) (Nevers (70.0 0.0)) 
    (Limoges (100.0 0.0)) (Roenne (85.0 0.0)) (Lyon (105.0 0.0))
    (Toulouse (130.0 0.0)) (Avignon (135.0 0.0)) (Marseille (145.0 0.0))))

(defparameter *origin* 'Marseille)

(defparameter *destination* '(Calais))

(defparameter *forbidden*  '(Avignon))

(defparameter *mandatory* '(Paris))


;; 
;; BEGIN: Exercise 1 -- Evaluation of the heuristics
;;
;; Returns the value of the heuristics for a given state
;;
;;  Input:
;;    state: the current state (vis. the planet we are on)
;;    sensors: a sensor list, that is a list of pairs
;;                (state (time-est cost-est) )
;;             where the first element is the name of a state and the second
;;             a number estimating the costs to reach the goal
;;
;;  Returns:
;;    The cost (a number) or NIL if the state is not in the sensor list
;;
;;  It is necessary to define two functions: the first which returns the 
;;  estimate of teh travel time, the second which returns the estimate of 
;;  the cost of travel

(defun f-h-time (state sensors)
  (first (first (rest (assoc state sensors)))))

(defun f-h-price (state sensors)
  (first (rest (first (rest (assoc state sensors))))))

;;
;; END: Exercise 1 -- Evaluation of the heuristic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 2 -- Navigation operators
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General navigation function
;;
;;  Returns the actions that can be carried out from the current state
;;
;;  Input:
;;    state:      the state from which we want to perform the action 
;;    lst-edges:  list of edges of the graph, each element is of the 
;;                form: (source destination (cost1 cost2))
;;    c-fun:      function that extracts the correct cost (time or price)
;;                from the pair that appears in the edge
;;    name:       name to be given to the actions that are created (see the 
;;                action structure)
;;    forbidden-cities:  
;;                list of the cities where we can't arrive by train
;;
;;  Returns
;;    A list of action structures with the origin in the current state and
;;    the destination in the states to which the current one is connected
;;
(defun nav-aux (state edge cfun name forbidden)
  (let
      ((origin (first edge))
       (dest (first (rest edge)))
       (cost (first (rest (rest edge)))))
    (when (and (eql state origin)
               (null (member dest forbidden)))
      (make-action :name name
                   :origin origin
                   :final dest
                   :cost (funcall cfun cost)))))

;; Funcion recursiva
(defun navigate (state lst-edges cfun name &optional forbidden)
  (elimina-nil (mapcar (lambda(edge) (nav-aux state edge cfun name forbidden)) lst-edges)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Navigation by canal
;;
;; This is a specialization of the general navigation function: given a 
;; state and a list of canals, returns a list of actions to navigate
;; from the current city to the cities reachable from it by canal navigation.
;;
(defun navigate-canal-time (state canals)
  (navigate state canals #'car 'navigate-canal-time))

(defun navigate-canal-price (state canals)
  (navigate state canals #'cadr 'navigate-canal-price))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Navigation by train
;;
;; This is a specialization of the general navigation function: given a 
;; state and a list of train lines, returns a list of actions to navigate
;; from the current city to the cities reachable from it by train.
;; 
;; Note that this function takes as a parameter a list of forbidden cities.
;;
(defun navigate-train-time (state trains forbidden)
  (navigate state trains #'car 'navigate-train-time forbidden))
  
(defun navigate-train-price (state trains forbidden)
  (navigate state trains #'cadr 'navigate-train-price forbidden))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 3 -- Goal test
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Goal test
;;
;;  Returns T or NIl depending on whether a path leads to a final state
;;
;;  Input:
;;    node:       node structure that contains, in the chain of parent-nodes,
;;                a path starting at the initial state
;;    destinations: list with the names of the destination cities
;;    mandatory:  list with the names of the cities that is mandatoryu to visit
;;
;;  Returns
;;    T: the path is a valid path to the final state
;;    NIL: invalid path: either the final city is not a destination or some
;;         of the mandatory cities are missing from the path.
;;

(defun contains-name-of (list-of-names node)
  (cond
   ((null list-of-names) NIL)
   ((eql (first list-of-names) (node-state node)) T)
   (T (contains-name-of (rest list-of-names) node))))
                         
(defun list-remove-elt (list elt)
  (cond
   ((null list) NIL)
   ((eql elt (first list)) (list-remove-elt (rest list) elt))
   (T (cons (first list)
            (list-remove-elt (rest list) elt)))))


(defun check-went-to-mandatories (node mandatory)
  (cond
   ((null node) NIL)
   ((null mandatory) T)
   ((contains-name-of mandatory node)
    (check-went-to-mandatories node (list-remove-elt mandatory (node-state node))))
   ((null (node-parent node)) NIL)
   (T (check-went-to-mandatories (node-parent node) mandatory))))


(defun f-goal-test (node destination mandatory)
  (if (null (contains-name-of destination node))
      NIL
    (check-went-to-mandatories node mandatory)))
   

;;
;; END: Exercise 3 -- Goal test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 4 -- Equal predicate for search states
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Determines if two nodes are equivalent with respect to the solution
;; of the problem: two nodes are equivalent if they represent the same city 
;; and if the path they contain includes the same mandatory cities.
;;  Input:
;;    node-1, node-2: the two nodes that we are comparing, each one
;;                    defining a path through the parent links
;;    mandatory:  list with the names of the cities that is mandatory to visit
;;
;;  Returns
;;    T: the two nodes are equivalent
;;    NIL: The nodes are not equivalent
;;
  
(defun has-visited (node mandatory-name)
  ;(contains-name-of (nodes-to-names (get-visited-nodes node)) mandatory-name))
  (cond
   ((null node) NIL)
   ((null (node-parent node)) NIL)
   ((eql (node-state node) mandatory-name) T)
   (T (has-visited (node-parent node) mandatory-name))))
  

; Esta funcion devuelve los nodos visitados en el orden
; que estan en mandatory
(defun get-visited-mandatories (node mandatory)
  (cond
   ((null mandatory) NIL)
   ((has-visited node (first mandatory))
    (cons (first mandatory)
          (get-visited-mandatories node (rest mandatory))))
   (T (get-visited-mandatories node (rest mandatory)))))
  
  
  
  

; En esta funcion sabemos que las listas de nombres tienen el mismo orden
; de elementos, por lo que solo comprobamos los primeros elementos de
; cada lista
(defun compare-list-of-names (list1 list2)
  (cond
   ((and (null list1) (null list2)) T)
   ((null list2) NIL)
   ((null list1) NIL)
   ((eql (first list1) (first list2))
    (compare-list-of-names (rest list1) (rest list2)))
   (T NIL)))
  
  
(defun f-search-state-equal (node-1 node-2 &optional mandatory)
  (cond
   ((not (eql (node-state node-1) (node-state node-2))) NIL)
   ((null mandatory) T)
   (T (compare-list-of-names
       (get-visited-mandatories node-1 mandatory)
       (get-visited-mandatories node-2 mandatory)))))

;;
;; END: Exercise 4 -- Equal predicate for search states
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BEGIN: Exercise 5 -- Define the problem structure
;;
;;
;;  Note that the connectivity of the netowrk using canals and trains
;;  holes is implicit in the operators: there is a list of two
;;  operators, each one takes a single parameter: a state name, and
;;  returns a list of actions, indicating to which states one can move
;;  and at which cost. The lists of edges are placed as constants as
;;  the second parameter of the navigate operators. 
;;
;;  There are two problems defined: one minimizes the travel time,
;;  the other minimizes the cost

(defun operator-aux (edges cost-f node)
  (if (null edges)
      NIL
    (let
        ((edge (first edges)))
      (let
         ((edge-state (first edge))
         (edge-cost (first (rest edge))))
      (if (eql edge-state (node-state node))
        (cons
         (cons edge-state 
               (cons (funcall cost-f edge-cost) NIL))
         (operator-aux (rest edges) cost-f node))
      (operator-aux (rest edges) cost-f node))))))
    

(defparameter *travel-cheap*
  (make-problem
   :states *cities*
   :initial-state *origin*
   :f-h #'(lambda (state)
            (f-h-cost state *estimate*))
   :f-goal-test #'(lambda (node)
                    (f-goal-test node *mandatory*))
   :f-search-state-equal #'(lambda (node-1 node-2)
                             (f-search-state-equal node-1 node-2 *mandatory*))
   :operators (list
               #'(lambda (node)
                   (operator-aux *trains* 'cadr node))
               #'(lambda (node)
                   (operator-aux *canals* 'cadr node))
               )
   )
  )


(defparameter *travel-fast* 
  (make-problem
   :states *cities*
   :initial-state *origin*
   :f-h #'(lambda (state)
            (f-h-time state *estimate*))
   :f-goal-test #'(lambda (node)
                    (f-goal-test node *mandatory*))
   :f-search-state-equal #'(lambda (node-1 node-2)
                             (f-search-state-equal node-1 node-2 *mandatory*))
   :operators (list
               #'(lambda (node)
                   (operator-aux *trains* 'car node))
               #'(lambda (node)
                   (operator-aux *canals* 'car node))
               )
   )
  )

;;
;;  END: Exercise 5 -- Define the problem structure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN Exercise 6: Expand node
;;
;; The main function of this section is "expand-node", which receives
;; a node structure (the node to be expanded) and a problem structure.
;; The problem structure has a list of navigation operators, and we
;; are interested in the states that can be reached using anuy one of
;; them.
;;
;; So, in the expand-node function, we iterate (using mapcar) on all
;; the operators of the problem and, for each one of them, we call
;; expand-node-operator, to determine the states that can be reached
;; using that operator.
;;
;; The operator gives us back a list of actions. We iterate again on
;; this list of action and, for each one, we call expand-node-action
;; that creates a node structure with the node that can be reached
;; using that action.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;  Creates a list with all the nodes that can be reached from the
;;  current one using all the operators in a given problem
;;
;;  Input:
;;    node:   the node structure from which we start.
;;    problem: the problem structure with the list of operators
;;
;;  Returns:
;;    A list (node_1,...,node_n) of nodes that can be reached from the
;;    given one
;;

(defun create-destination-nodes (node destination name)
  (cond
   ((null destinations) NIL)
   (T
    (let
        ((dest (first destinations)))
      (let
          ((dest-state (first dest))
           (dest-cost (first (rest dest)))
           (g-value (+ (node-g node) (first (rest dest))))
           (h-value (funcall (problem-f-h (first dest)))))
        (make-node
         :state (first dest)
         :parent node
         :action (make-action
                  :name name
                  :origin (node-state node)
                  :final dest-state
                  :cost dest-cost)
         :depth (+ (node-depth node) 1)
         :g g-value
         :h h-value
         :f (+ g-value h-value)))))))
       
   

(defun expand-node-train (node problem)
  (if (null node)
      NIL
    (let
        ((destinations (funcall (first (problem-operators problem)) node)))
      (create-destination-nodes node destinations 'navigate-train))))

(defun expand-node-canal (node problem)
  (if (null node)
      NIL
    (let
        ((destinations (funcall (first (rest (problem-operators problem))) node)))
      (create-destination-nodes node destinations 'navigate-canal))))
    

(defun expand-node (node problem)
  (append
   (expand-node-train node problem)
   (expand-node-canal node problem)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  BEGIN Exercise 7 -- Node list management
;;;  
;;;  Merges two lists of nodes, one of them ordered with respect to a
;;;  given strategy, keeping the result ordered with respect to the
;;;  same strategy.
;;;
;;;  This is the idea: suppose that the ordering is simply the
;;;  ordering of natural numbers. We have a "base" list that is
;;;  already ordered, for example:
;;;      lst1 --> '(3 6 8 10 13 15)
;;;
;;;  and a list that is not necessarily ordered:
;;;
;;;      nord --> '(11 5 9 16)
;;;
;;;  the call (insert-nodes nord lst1 #'<) would produce
;;;
;;;    (3 5 6 8 9 10 11 13 15 16)
;;;
;;;  The functionality is divided in three functions. The first,
;;;  insert-node, inserts a node in a list keeping it ordered. The
;;;  second, insert-nodes, insert the nodes of the non-ordered list
;;;  into the ordered, one by one, so that the two lists are merged.
;;;  The last function, insert-node-strategy is a simple interface that
;;;  receives a strategy, extracts from it the comparison function, 
;;;  and calls insert-nodes


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Inserts a list of nodes in an ordered list keeping the result list
;; ordered with respect to the given comparison function
;;
;; Input:
;;    nodes: the (possibly unordered) node list to be inserted in the
;;           other list
;;    lst-nodes: the (ordered) list of nodes in which the given nodes 
;;               are to be inserted
;;    node-compare-p: a function node x node --> 2 that returns T if the 
;;                    first node comes first than the second.
;;
;; Returns:
;;    An ordered list of nodes which includes the nodes of lst-nodes and 
;;    those of the list "nodes@. The list is ordered with respect to the 
;;   criterion node-compare-p.
;; 
(defun insert-nodes (nodes lst-nodes node-compare-p)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Inserts a list of nodes in an ordered list keeping the result list
;; ordered with respect the given strategy
;;
;; Input:
;;    nodes: the (possibly unordered) node list to be inserted in the
;;           other list
;;    lst-nodes: the (ordered) list of nodes in which the given nodes 
;;               are to be inserted
;;    strategy: the strategy that gives the criterion for node
;;              comparison
;;
;; Returns:
;;    An ordered list of nodes which includes the nodes of lst-nodes and 
;;    those of the list "nodes@. The list is ordered with respect to the 
;;    criterion defined in te strategy.
;; 
;; Note:
;;   You will note that this function is just an interface to
;;   insert-nodes: it allows to call using teh strategy as a
;;   parameter; all it does is to "extract" the compare function and
;;   use it to call insert-nodes.
;;
(defun insert-nodes-strategy (nodes lst-nodes strategy)
  )

;;
;;    END: Exercize 7 -- Node list management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 8 -- Definition of the A* strategy
;;
;; A strategy is, basically, a comparison function between nodes to tell 
;; us which nodes should be analyzed first. In the A* strategy, the first 
;; node to be analyzed is the one with the smallest value of g+h
;;
(defparameter *A-star*
  (make-strategy ))

;;
;; END: Exercise 8 -- Definition of the A* strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 9: Search algorithm
;;;
;;;    Searches a path that solves a given problem using a given search
;;;    strategy. Here too we have two functions: one is a simple
;;;    interface that extracts the relevant information from the
;;;    problem and strategy structure, builds an initial open-nodes
;;;    list (which contains only the starting node defined by the
;;;    state), and initial closed node list (the empty list), and calls
;;;    the auxiliary function.
;;;
;;;    The auxiliary is a recursive function that extracts nodes from
;;;    the open list, expands them, inserts the neighbors in the
;;;    open-list, and the expanded node in the closed list. There is a
;;;    caveat: with this version of the algorithm, a node can be
;;;    inserted in the open list more than once. In this case, if we
;;;    extract a node in the open list and the following two condition old:
;;;
;;;     the node we extract is already in the closed list (it has
;;;     already been expanded)
;;;       and
;;;     the path estimation that we have is better than the one we
;;;     obtain from the node in the open list
;;;
;;;     then we ignore the node.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Auxiliary search function (the one that actually does all the work
;;
;;  Input:
;;    problem: the problem structure from which we get the general 
;;             information (goal testing function, action operatos, etc.
;;    open-nodes: the list of open nodes, nodes that are waiting to be
;;                visited
;;    closed-nodes: the list of closed nodes: nodes that have already
;;                  been visited
;;    strategy: the strategy that decide which node is the next extracted
;;              from the open-nodes list
;;
;;    Returns:
;;     NIL: no path to the destination nodes
;;     If these is a path, returns the node containing the final state.
;;
;;     Note that what is returned is quite a complex structure: the
;;     node contains in "parent" the node that comes before in the
;;     path, that contains another one in "parents" and so on until
;;     the initial one. So, what we have here is a rather complex
;;     nested structure that contains not only the final node but the
;;     whole path from the starting node to the final.
;;
(defun graph-search-aux (problem open-nodes closed-nodes strategy)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Interface function for the graph search. 
;;
;;  Input:
;;    problem: the problem structure from which we get the general 
;;             information (goal testing function, action operatos,
;;             starting node, heuristic, etc.
;;    strategy: the strategy that decide which node is the next extracted
;;              from the open-nodes list
;;
;;    Returns:
;;     NIL: no path to the destination nodes
;;     If these is a path, returns the node containing the final state.
;;
;;    See the graph-search-aux for the complete structure of the
;;    returned node. 
;;    This function simply prepares the data for the auxiliary
;;    function: creates an open list with a single node (the source)
;;    and an empty closed list.
;;
(defun graph-search (problem strategy)
  )

;
;  A* search is simply a function that solves a problem using the A* strategy
;
(defun a-star-search (problem)
  )


;;
;; END: Exercise 9 -- Search algorithm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 10: Solution path
;;;
;*** solution-path ***

(defun solution-path (node)
  )

;*** action-sequence ***
; Visualize sequence of actions

(defun action-sequence (node)
  )

;;; 
;;;    END Exercise 10: Solution path / action sequence
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
