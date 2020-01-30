(defpackage game
  (:documentation "A package to handle the main game logic.")
  (:use :cl)
  (:import-from :2d
                :circle :vect
                :x :y :radius)
  (:export :play :*win-width* :*win-height*))
(in-package :game)

(defparameter *win-width* 1080
  "The width of the window/game (in pixels).")
(defparameter *win-height* 720
  "The height of the window/game (in pixels).")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOIDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defparameter *super-boid* (make-instance 'boid
                                          :x 42 :y 42 :radius 30))

(defparameter *gang-size* 500)
(defparameter *boid-gang* (loop repeat *gang-size* collect (make-random-boid)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SDL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (defparameter *textures-dir* "/home/mc/common-lisp/swarm/textures/")
(defparameter *fps* 60)


(defun init-win ()
  "Initialize the SDL window."
  (sdl:window *win-width* *win-height*
              :title-caption "Swarm"
              :flags '(sdl:sdl-hw-surface sdl:sdl-doublebuf))
  (setf (sdl:frame-rate) *fps*))

(defun event-loop ()
  "Handle the SDL events (keyboard mostly)."
  (sdl:with-events ()
    (:quit-event () t)

    (:video-expose-event ()
                         (sdl:update-display))

    (:key-down-event (:key key)
                     (when (sdl:key= key :sdl-key-escape)
                       (sdl:push-quit-event)))

    (:mouse-button-down-event (:button button) ; :state state :x x :y y)
                              (when (sdl:key= button sdl:sdl-button-left)
                                (setf (*color* *super-boid*)
                                      (make-random-color))))

    (:idle ()
           (sdl:clear-display sdl:*black*)
           (mapc #'display *boid-gang*)
           (display *super-boid*)
           (with-slots (x y) *super-boid*
             (setf x (sdl:mouse-x))
             (setf y (sdl:mouse-y)))
           (sdl:update-display))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ENTRY POINT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun play ()
  "This is the entry point of the swarm simulator."
  (sdl:with-init ()
    (init-win)

    ;; (sdl:draw-surface (sdl:load-image (concatenate 'string
    ;;                                                *textures-dir* "xkcd.bmp")))
    ;; (sdl:update-display)

    (event-loop)))
