(in-package :2d)

(defparameter *world-width* 1080
  "The width of the window/game (in pixels).")
(defparameter *world-height* 720
  "The height of the window/game (in pixels).")


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

(defmethod distance ((self point) (rhs point))
  "Return the distance between 2 points."
  (with-slots ((l-x x) (l-y y)) self
    (with-slots ((r-x x) (r-y y)) rhs
      (let ((x-diff (- r-x l-x))
            (y-diff (- r-y l-y)))
        (sqrt (+ (* x-diff x-diff)
                 (* y-diff y-diff)))))))

(defmethod find-points-in-range ((self point) (point-list list) range)
  "Return all the points from POINT-LIST which are not further than RANGE pixels to POINT."
  (remove-if (lambda (p) (> (distance self p) range)) point-list))


(defclass circle (point)
  ((radius
    :initarg :radius
    :initform 1
    :accessor *radius*
    :type integer
    :documentation "Radius of the circle."))
  (:documentation "A circle."))


(defclass vect (point)
  ((direction
    :initarg :direction
    :initform (make-instance 'point)
    :accessor *direction*
    :type point
    :documentation "A direction point, assuming SELF is at (0, 0)."))
  (:documentation "A moving point."))

(defmethod move ((self vect))
  "Update the VECT coordinates accordingly to its direction."
  (declare (optimize (speed 3) (safety 2)))
  (with-slots (x y direction) self
    (setf x
          (mod
           (+ x (*x* direction))
           *world-width*))
    (setf y
          (mod
           (+ y (*y* direction))
           *world-height*))))
