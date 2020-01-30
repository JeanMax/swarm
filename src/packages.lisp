(defpackage 2d
  (:documentation "A package to handle all the 2d stuffs... MATHS!")
  (:use :cl)
  (:export :point :circle :vect
           :x :y :radius))


(defpackage game
  (:documentation "A package to handle the main game logic.")
  (:use :cl)
  (:import-from :2d
                :circle :vect
                :x :y :radius)
  (:export :play ))
