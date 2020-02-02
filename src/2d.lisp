(in-package :2d)
(declaim (optimize (speed 3) (debug 3)))

(declaim (type (unsigned-byte 16) *world-width* *world-height* *tile-size*))
(defparameter *world-width* 1280
  "The width of the window/game (in pixels).")
(defparameter *world-height* 720
  "The height of the window/game (in pixels).")

(defparameter *tile-size* 100)
(declaim (type (simple-array list) *grid*))
(defparameter *grid* (make-array (list (ceiling *world-height* *tile-size*)
                                       (ceiling *world-width* *tile-size*))
                                 :initial-element nil))

(defun reset-grid ()
  (let ((max-y (first (array-dimensions *grid*)))
        (max-x (second (array-dimensions *grid*))))
    (declare (type (unsigned-byte 16) max-x max-y))
    (loop for y from 0 below max-y do
      (loop for x from 0 below max-x do
           (setf (aref *grid* y x) nil)))))

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

(defmethod add ((self point) (rhs point))
  (with-slots ((l-x x) (l-y y)) self
    (declare (type (signed-byte 16) l-x l-y))
    (with-slots ((r-x x) (r-y y)) rhs
      (declare (type (signed-byte 16) r-x r-y))
      (setf l-x (+ l-x r-x))
      (setf l-y (+ l-y r-y))))
  self)

(defmethod sub ((self point) (rhs point))
  (with-slots ((l-x x) (l-y y)) self
    (declare (type (signed-byte 16) l-x l-y))
    (with-slots ((r-x x) (r-y y)) rhs
      (declare (type (signed-byte 16) r-x r-y))
      (setf l-x (- l-x r-x))
      (setf l-y (- l-y r-y))))
  self)

(defmethod mul ((self point) scalar)
  (declare (type (signed-byte 16) scalar))
  (with-slots ((l-x x) (l-y y)) self
    (declare (type (signed-byte 16) l-x l-y))
    (setf l-x (* l-x scalar))
    (setf l-y (* l-y scalar)))
  self)

(defmethod div ((self point) scalar)
  (declare (type (signed-byte 16) scalar))
  (with-slots ((l-x x) (l-y y)) self
    (declare (type (signed-byte 16) l-x l-y))
    (setf l-x (round l-x scalar))
    (setf l-y (round l-y scalar)))
  self)

(declaim (inline add-to-grid))
(defmethod add-to-grid ((self point))
  (with-slots ((l-x x) (l-y y)) self
    (declare (type (signed-byte 16) l-x l-y))
    (push self (aref *grid*
                     (truncate l-y *tile-size*)
                     (truncate l-x *tile-size*)))))

(defmethod find-points-maybe-in-range ((self point) range)
  (declare (type (signed-byte 16) range))
  (with-slots ((l-x x) (l-y y)) self
    (declare (type (signed-byte 16) l-x l-y))
    (let ((ret nil)
          (offset (ceiling range *tile-size*))
          (grid-y (truncate l-y *tile-size*))
          (grid-x (truncate l-x *tile-size*))
          (max-y (first (array-dimensions *grid*)))
          (max-x (second (array-dimensions *grid*))))
      (declare (type (signed-byte 16) offset grid-x grid-y max-x max-y))
      ;; (break)                           ;DEBUG
      (loop for y from (- grid-y offset) below (+ grid-y offset 1) do
           (loop for x from (- grid-x offset) below (+ grid-x offset 1) do
                (when (and (< -1 y max-y) (< -1 x max-x))
                    (setf ret (append (aref *grid* y x) ret)))))
      ret)))

(declaim (inline distance))
(declaim (ftype (function (point point) (unsigned-byte 16)) distance))
(defmethod distance ((self point) (rhs point))
  "Return the distance between 2 points."
  (with-slots ((l-x x) (l-y y)) self
    (declare (type (signed-byte 16) l-x l-y))
    (with-slots ((r-x x) (r-y y)) rhs
      (declare (type (signed-byte 16) r-x r-y))
      (let ((x-diff (- r-x l-x))
            (y-diff (- r-y l-y)))
        (round (sqrt (+ (* x-diff x-diff)
                        (* y-diff y-diff))))))))

(defmethod find-points-in-range--slow ((self point) (point-list list) range
                                       include-self)
  "Return all the points from POINT-LIST which are not further than RANGE pixels to POINT."
  (declare (type (unsigned-byte 16) range))
  (remove-if (lambda (p) (or (> (distance self p) range)
                             (and (not include-self) (eq p self))))
             point-list))

(defmethod find-points-in-range ((self point) range
                                 &optional (include-self nil))
  "Return all the points from POINT-LIST which are not further than RANGE pixels to POINT."
  (declare (type (unsigned-byte 16) range))
  (find-points-in-range--slow self
                              (find-points-maybe-in-range self range)
                              range
                              include-self))


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
    (declare (type (signed-byte 16) x y))
    (with-slots ((x-dir x) (y-dir y)) direction
      (declare (type (signed-byte 16) x-dir y-dir))
      (setf x (mod (+ x x-dir) *world-width*))
      (setf y (mod (+ y y-dir) *world-height*))
      (set-coords previous-direction x-dir y-dir)
      (add-to-grid self))))
