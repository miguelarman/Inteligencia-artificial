;;;;;;;;;;;;;;;;; Recursive
(defun norma-rec (x)
	(if(null x)
		0
		(+
			(* (first x) (first x))
			(norma-rec (rest x)))))

(defun producto-escalar-rec (x y)
	(cond
		((null x) 0)
		((null y) 0)
		(T
			(+ 
				(* (first x) (first y))
				(producto-escalar-rec (rest x) (rest y))))))

(defun cosine-distance-rec (x y)
	(let
		((nx (norma-rec x))
		 (ny (norma-rec y)))
	(cond
		((and (= nx 0)
			  (= ny 0))
				0)
		((= nx 0) 1)
		((= ny 0) 1)
		(T (- 1
				(/  (producto-escalar-rec x y)
					(*  (sqrt nx)
		 			 	(sqrt ny))))))))

;;;;;;;;;; Mapcar

(defun norma-mapcar (x)
	(apply #'+
		(mapcar #'* x x)))

(defun producto-escalar-mapcar (x y)
	(cond
		((null x) 0)
		((null y) 0)
		(T
			(apply #'+
				(mapcar #'* x y)))))

(defun cosine-distance-mapcar (x y)
	(let
		((nx (norma-mapcar x))
		 (ny (norma-mapcar y)))
	(cond
		((and (= nx 0)
			  (= ny 0))
				0)
		((= nx 0) 1)
		((= ny 0) 1)
		(T (- 1
				(/  (producto-escalar-mapcar x y)
					(*  (sqrt nx)
		 			 	(sqrt ny))))))))

(setf lsta '(1 2 3))
(setf lstb '(2 3 4))

(= (cosine-distance-rec lsta lstb) (cosine-distance-mapcar lsta lstb))
	
