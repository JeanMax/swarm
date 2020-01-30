(in-package :game)


;; TODO: find a better way for these
(defparameter *win-width* 1080
  "The width of the window/game (in pixels).")
(defparameter *win-height* 720
  "The height of the window/game (in pixels).")

(setf *random-state* (make-random-state t))  ; random seed


(defclass boid (circle vect)
  ((color
    :initarg :color
    :initform sdl:*white*
    :accessor *color*)))

(defmethod display ((self boid))
  (with-slots (x y radius color) self
    (sdl:draw-filled-circle-* x y radius
                              :color color)))


(defun make-random-color ()
  (sdl:color :r (random 255)
             :g (random 255)
             :b (random 255)))

;; TODO: the random distribution sucks
(defun make-random-boid ()
  (let ((min-radius 3)
        (max-radius 7))
    (make-instance 'boid
                   :x (+ max-radius
                         (random  (- *win-width*
                                     (* 2 max-radius))))
                   :y (+ max-radius
                         (random  (- *win-height*
                                     (* 2 max-radius))))
                   :radius (+ min-radius (random  max-radius))
                   :color (make-random-color))))
