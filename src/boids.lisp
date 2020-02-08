(in-package :swarm)
(declaim (optimize (speed 3) (safety 1) (debug 3)))

(declaim (type (unsigned-byte 8) +boid-sight-range+ +boid-max-speed+))
; TODO: the relation with +tile-size+ should be the other way around
(defparameter +boid-sight-range+ (/ +tile-size+ 2)
  "The height of the window/game (in pixels).")
(defparameter +boid-sight-color+ (sdl:color :r 255 :a 20)
  "The height of the window/game (in pixels).")

(defconstant +boid-max-speed+ 10)

(declaim (type single-float *alignment-coef* *cohesion-coef* *separation-coef*))
(defparameter *alignment-coef* 3.33)
(defparameter *cohesion-coef* 0.9)
(defparameter *separation-coef* 0.9)

(defconstant +angle-prout+ (/ (* 3 PI) 4))
(defconstant +angle-pouet+ (- +angle-prout+))
(defconstant +sin-b+ (sin +angle-prout+))
(defconstant +cos-b+ (cos +angle-prout+))
(defconstant +sin-a+ (sin +angle-pouet+))
(defconstant +cos-a+ (cos +angle-pouet+))

(defun mean-coord (direction-list)
  (let ((n-directions (list-length direction-list)))
    (when (> n-directions 0)
      (labels ((mean-axis (axis-getter)
                 (round (reduce #'+ (mapcar axis-getter direction-list))
                        n-directions)))
        ; TODO: don't allocate a point each time
        (make-instance 'point :x (mean-axis #'*x*) :y (mean-axis #'*y*))))))

(defun alignment-force (neighbors)
  (mulf
   (mean-coord
    (mapcar (lambda (n) (*previous-direction* n)) neighbors))
   *alignment-coef*))

(defun cohesion-force (self neighbors)
  (mulf (sub (mean-coord neighbors) self) *cohesion-coef*))

(defun separation-force (self neighbors)
  (with-slots ((l-x x) (l-y y)) self
    (declare (type (signed-byte 16) l-x l-y))
    (mulf (mean-coord (mapcar
                       (lambda (n) (sub (make-instance 'point :x l-x :y l-y) n))
                       neighbors))
          *separation-coef*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOID ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass boid (circle vect)
  ((color
    :initarg :color
    :initform sdl:*white*
    :accessor *color*
    :type sdl:color
    :documentation "The color of the boid."))
  (:documentation "A boid (see: https://en.wikipedia.org/wiki/Boids)."))



(defmethod display ((self boid))
  "Display the given BOID on the sdl window. (Special cace-dedi to Pierrot le ouf!)"
  (with-slots (x y direction radius color) self
    (sdl:draw-filled-circle-* x y +boid-sight-range+ :color +boid-sight-color+)
    ;; (let* ((x-dir (*x* direction))
    ;;        (y-dir (*y* direction))

    ;;        (x-a (+ x x-dir))
    ;;        (y-a (+ y y-dir))  ; TODO: cut

    ;;        (x-b (+ (* x-dir +cos-a+)      (* y-dir +sin-a+)))
    ;;        (y-b (+ (* x-dir (- +sin-a+))) (* y-dir +cos-a+))

    ;;        (x-c (+ (* x-dir +cos-b+)      (* y-dir +sin-b+)))
    ;;        (y-c (+ (* x-dir (- +sin-b+))) (* y-dir +cos-b+)))

    ;;        (if (= 0 x-dir y-dir)
    ;;            (sdl:draw-filled-circle-* x y radius :color color)

    ;;            (sdl:draw-filled-trigon
    ;;             (sdl:point :x x-a :y y-a)
    ;;             (sdl:point :x (+ x-a x-b) :y (+ y-a y-b))
    ;;             (sdl:point :x (+ x-a x-c) :y (+ y-a y-c))
    ;;             :color color)))))
               (sdl:draw-filled-circle-* x y radius :color color)))


(defmethod apply-forces ((self boid))
  (let ((neighbors
          (find-points-in-range self +boid-sight-range+))
        (new-direction nil))
    (unless (null neighbors)
      (setf new-direction
            (mean-coord (list (*direction* self)
                              (alignment-force neighbors)
                              (separation-force self neighbors)
                              (cohesion-force self neighbors))))
      (set-coords (*direction* self)
                  (rem (the integer (*x* new-direction)) +boid-max-speed+)
                  (rem (the integer (*y* new-direction)) +boid-max-speed+)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RANDOM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setf *random-state* (make-random-state t))  ; random seed

(defun make-random-color ()
  "Return a randomly generated color."
  (sdl:color :r (random 255)
             :g (random 255)
             :b (random 255)))

;; TODO: the random distribution sucks
(defun random-interval (min-val max-val)
  "Return a random int in the interval [MIN-VAL, MAX-VAL]."
  (declare (type (signed-byte 16) min-val max-val))
  (+ (random (- max-val min-val -1)) min-val))

(defun make-random-boid ()
  "Return a fresh boid initialized with random attributes in a reasonable range."
  (let ((min-radius 3)
        (max-radius 5)
        (max-speed 4))
    (declare (type (unsigned-byte 4) min-radius max-radius max-speed))
    (make-instance
     'boid
     :x (random-interval max-radius (- +world-width+ max-radius))
     :y (random-interval max-radius (- +world-height+ max-radius))
     :radius (random-interval min-radius max-radius)
     :color (make-random-color)
     :direction (make-instance
                 'point
                 :x (random-interval (- max-speed) max-speed)
                 :y (random-interval (- max-speed) max-speed)))))
