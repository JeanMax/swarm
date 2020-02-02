(defsystem "swarm"
  :version "0.1.0"
  :author "JeanMax"
  :license "LLGPL"
  :depends-on ("lispbuilder-sdl" "unix-opts")
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "2d")
                             (:file "boids") ; :depends-on ("main"))
                             (:file "main"))))
  :description "A swarm simulator."
  :in-order-to ((test-op (test-op "swarm/tests"))))

(defsystem "swarm/tests"
  :author "JeanMax"
  :license "LLGPL"
  :depends-on ("swarm"
               "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :description "Test system for swarm"
  :perform (test-op (op c) (symbol-call :rove :run c)))
