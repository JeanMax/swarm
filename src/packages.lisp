(declaim (optimize (speed 3) (safety 1) (debug 3)))


(defpackage 2d
  (:documentation "A package to handle all the 2d stuffs... MATHS!")
  (:use :cl)
  (:export :point :circle :vect
           :x :y :radius :direction
           :*x* :*y* :*direction* :*previous-direction*  ; TODO: don't use muffle here
           :+world-width+ :+world-height+ :+tile-size+
           :set-coords :add :sub :mulf :mul :div :move
           :reset-grid :find-points-in-range
           :*boxed-world*))


(defpackage swarm
  (:documentation "A package to handle the main game logic.")
  (:use :cl)
  (:import-from :2d
                :point :circle :vect
                :x :y :radius :direction
                :*x* :*y* :*direction* :*previous-direction*
                :*boxed-world*
                :+world-width+ :+world-height+ :+tile-size+
                :set-coords :add :sub :mulf :mul :div :move
                :reset-grid  :find-points-in-range)
  (:export :play
           :*alignment-coef* :*cohesion-coef* :*separation-coef*
           :*gang-size* :*nproc*))
