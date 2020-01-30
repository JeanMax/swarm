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
  (
   ;; (speed
   ;;  :initarg :speed
   ;;  :initform 0
   ;;  :accessor *speed*
   ;;  :type float
   ;;  :documentation "Speed of the point in 'pixel per frame'.")
   (direction
    :initarg :direction
    :initform (make-instance 'point)
    :accessor *direction*
    :type point
    :documentation "A direction point, assuming SELF is at (0, 0)."))
  (:documentation "A moving point."))

(defmethod move ((self vect))
  (with-slots (x y direction) self
    (incf x (*x* direction))
    (incf y (*y* direction))))
