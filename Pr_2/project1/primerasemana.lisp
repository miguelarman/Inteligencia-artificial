;; Ciudades: Lista constante de símbolos representando los nombres de las ciudades
(defparameter *cities* '(Calais
                         ReimsParis Nancy Orleans
                         St. Malo Nantes Brest Nevers
                         Limoges Roenne Lyon Toulouse
                         Avignon Marseille))

;; Las conexiones ferroviarias y por canales. Cada una es una lista de triplas: el primer elemento de la tripla
;; es la ciudad origen, el segundo es la ciudad destino, y el tercero es una lista con dos elementos: el tiempo
;; de recorrido y el coste. Los arcos no dirigidos de la red de trenes se modelan como pares de arcos con el
;; mismo coste, uno por cada dirección:
(defparameter *canals*
  '(
    (Paris Calais (50 20))
    (Paris Nevers (70 30))
    ;; ...
    ))

(defparameter *train*
  '(
    (Paris Calais (34 60)) (Calais Paris (34 60))
    (Paris Nevers (48 75)) (Nevers Paris (48 75))
    ;; ...
    ))

;; Las estimaciones (heurística): Lista constante de pares formados por una ciudad y dos correspondientes
;; valores de las heurísticas (tiempo de recorrido y coste)
(defparameter *estimate*
  '((Calais (0 0))
    (Reims (25 0))
    (Paris (30 0))
    (Nancy (50 0))
    (Orleans (55 0))
    (St. Malo (65 0))
    (Nantes (75 0))
    (Brest (90 0))
    (Nevers (70 0))
    (Limoges (100 0))
    (Roenne (85 0))
    (Lyon (105 0))
    (Toulouse (130 0))
    (Avignon (135 0))
    (Marseille (145 0))))

;; Ciudad origen: El estado inicial será el correspondiente a estar en la ciudad donde empieza el viaje.
(defparameter *origin* 'Marseille)

;; Ciudad destino: Lista constante de símbolos representando los nombres de las ciudades destino.
(defparameter *destination* '(Calais))

;; Ciudades prohibidas: Lista constante de símbolos con los nombres de las ciudades a que no está
;; permitido llegar en tren debido a obra en la estación.
(defparameter *forbidden-cities* '(Nevers))

;; Ciudades obligadas: Lista constante de símbolos con los nombres de las ciudades por donde es
;; obligatorio pasar para alcanzar la meta
(defparameter *mandatory-cities* '(Nantes Paris))
