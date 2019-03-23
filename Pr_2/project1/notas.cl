(defstruct action
  name
  origin
  final
  cost)

(make-action :name 'myaction
             :origin 'Paris
             :final 'Reims
             :cost 25.0)
