(declaim (optimize (speed 3) (debug 3)))


(defpackage 2d
  (:documentation "A package to handle all the 2d stuffs... MATHS!")
  (:use :cl)
  (:export :point :circle :vect
           :x :y :radius
           :*x* :*y* :*direction* :*previous-direction*  ; TODO: don't use muffle here
           :*world-width* :*world-height*
           :set-coords :add :sub :mul :div :move :find-points-in-range))


(defpackage swarm
  (:documentation "A package to handle the main game logic.")
  (:use :cl)
  (:import-from :2d
                :point :circle :vect
                :x :y :radius
                :*x* :*y* :*direction* :*previous-direction*
                :*world-width* :*world-height*
                :set-coords :add :sub :mul :div :move :find-points-in-range)
  (:export :play ))
