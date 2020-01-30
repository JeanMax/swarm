(in-package :2d)


(defclass point ()
  ((x
    :initarg :x
    :initform 0
    :accessor *x*
    :type integer
    :documentation "X axis coordinate.")
   (y
    :initarg :y
    :initform 0
    :accessor *y*
    :type integer
    :documentation "Y axis coordinate."))
  (:documentation "A 2d point."))


(defclass circle (point)
  ((radius
    :initarg :radius
    :initform 1
    :accessor *radius*
    :type integer
    :documentation "Radius of the circle."))
  (:documentation "A circle."))


(defclass vect (point)
  ((speed
    :initarg :speed
    :initform 0
    :accessor *speed*)
   (angle
    :initarg :angle
    :initform 0
    :accessor *angle*))
  (:documentation "A moving point."))
