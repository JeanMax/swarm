(in-package :2d)
(declaim (optimize (speed 3) (debug 3)))

(declaim (type (unsigned-byte 16) *world-width* *world-height*))
(defparameter *world-width* 1280
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

(defmethod set-coords ((self point) new-x new-y)
  (with-slots ((l-x x) (l-y y)) self
    (declare (type (signed-byte 16) l-x l-y new-x new-y))
    (setf l-x new-x)
    (setf l-y new-y)))

(declaim (inline distance))
(declaim (ftype (function (point point) (unsigned-byte 16)) distance))
(defmethod distance ((self point) (rhs point))
  "Return the distance between 2 points."
  (with-slots ((l-x x) (l-y y)) self
    (with-slots ((r-x x) (r-y y)) rhs
      (declare (type (signed-byte 16) l-x l-y r-x r-y))
      (let ((x-diff (- r-x l-x))
            (y-diff (- r-y l-y)))
        (round (sqrt (+ (* x-diff x-diff)
                        (* y-diff y-diff))))))))

(defmethod find-points-in-range ((self point) (point-list list) range
                                 &optional (include-self nil))
  "Return all the points from POINT-LIST which are not further than RANGE pixels to POINT."
  (declare (type (unsigned-byte 16) range))
  ;; TODO: optimize that with spatial partioning or something
  (remove-if (lambda (p) (or (> (distance self p) range)
                             (and (not include-self) (eq p self))))
             point-list))


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
    :documentation "A direction point, assuming SELF is at (0, 0).")
   (previous-direction
    :initarg :previous-direction
    :initform (make-instance 'point)
    :accessor *previous-direction*
    :type point
    :documentation "A copy of the previous direction, to allow global movement."))
  (:documentation "A moving point."))

(defmethod move ((self vect))
  "Update the VECT coordinates accordingly to its direction."
  (with-slots (x y direction previous-direction) self
    (with-slots ((x-dir x) (y-dir y)) direction
      (declare (type (signed-byte 16) x y x-dir y-dir))
      (setf x (mod (+ x x-dir) *world-width*))
      (setf y (mod (+ y y-dir) *world-height*))
      (set-coords previous-direction x-dir y-dir))))
