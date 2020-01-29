(defpackage swarm
  (:use :cl)
  (:export #:play))
(in-package :swarm)

(defparameter *textures-dir* "/home/mc/common-lisp/swarm/textures/")
(defparameter *win-width* 1080)
(defparameter *win-height* 720)
(defparameter *fps* 60)

(defparameter *random-color* sdl:*white*)

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

    (:mouse-button-down-event (:button button :state state :x x :y y)
                              (when (sdl:key= button sdl:sdl-button-left)
                                (setf *random-color* (sdl:color :r (random 255)
                                                                :g (random 255)
                                                                :b (random 255)))))

    (:idle ()
           ;; ;; Change the color of the box if the left mouse button is depressed
           ;; (when (sdl:mouse-left-p)
           ;;   (setf *random-color* (sdl:color :r (random 255)
           ;;                                   :g (random 255)
           ;;                                   :b (random 255))))
           (sdl:clear-display sdl:*black*)
           (sdl:draw-box (sdl:rectangle-from-midpoint-* (sdl:mouse-x)
                                                        (sdl:mouse-y)
                                                        20 20)
                         :color *random-color*)
           (sdl:update-display))))

(defun play ()
  "This is the entry point of the swarm simulator."
  (sdl:with-init ()
    (init-win)

    ;; (sdl:draw-surface (sdl:load-image (concatenate 'string
    ;;                                                *textures-dir* "xkcd.bmp")))
    ;; (sdl:update-display)

    (event-loop)))
