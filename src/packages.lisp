(defpackage 2d
  (:documentation "A package to handle all the 2d stuffs... MATHS!")
  (:use :cl)
  (:export :point :circle :vect
           :x :y :radius
           :move))


(defpackage swarm
  (:documentation "A package to handle the main game logic.")
  (:use :cl)
  (:import-from :2d
                :point :circle :vect
                :x :y :radius
                :move)
  (:export :play ))
