;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Breadth-first-search in graphs
;;;
(defun bfs (end queue net)
  (if (null queue) '()
    (let* ((path (first queue))
           (node (first path)))
      (if (eql node end)
          (reverse path)
        (bfs end
             (append (rest queue)
                     (new-paths path node net))
             net)))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
    (rest (assoc node net))))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun shortest-path (start end net)
  (bfs end (list (list start)) net))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lista-contiene (lista elt)
  (if (null (find elt lista))
      nil
    T))

(defun bfs-improved-aux (end queue net visitados)
  (if (null queue) '()
    (let* ((path (first queue))
           (node (first path)))
      (if (eql node end)
          (reverse path)
        (if (lista-contiene visitados node)
            (bfs-improved-aux end
                              (rest queue)
                              net
                              visitados)
          (bfs-improved-aux end
               (append (rest queue)
                       (new-paths path node net))
               net
               (cons node visitados)))))))

  
  
  
  
  
(defun bfs-improved (end queue net)
  (bfs-improved-aux end queue net nil))


(defun shortest-path-improved (end queue net)
  (bfs-improved end (list (list start)) net))
