(define-module (g2048 ui)
 ;; repl server
 #:use-module (system repl coop-server)
 ;; chickadee
 #:use-module (chickadee)
 #:use-module (chickadee audio)
 #:use-module (chickadee graphics color)
 #:use-module (chickadee graphics texture)
 #:use-module (chickadee graphics sprite)
 #:use-module (chickadee graphics text)
 #:use-module (chickadee graphics path)
 #:use-module (chickadee graphics particles)
 #:use-module (chickadee graphics tile-map)
 #:use-module (chickadee graphics viewport)
 #:use-module (chickadee math)
 #:use-module (chickadee math matrix)
 #:use-module (chickadee math rect)
 #:use-module (chickadee math vector)
 #:use-module (chickadee scripting)
 ;; the cli module
 #:use-module (g2048 cli)
 #:export (start)
 )

;; repl server
(define repl (spawn-coop-repl-server))

;; some wrapper
(define (draw-wrap alpha)
  (draw alpha))
(define (update-wrap dt)
  (poll-coop-repl-server repl)
  (update dt))

;;; main procedures

(define (load)
  (if #f #f))

(define (draw alpha)
  (if #f #f))

(define (update dt)
  ;; update global agenda
  (update-agenda dt))

(define (start)
  (run-game #:window-title "2048"
	    #:window-width 480
	    #:window-height 640
	    #:window-fullscreen? #f
	    #:window-resizable? #f
	    #:window-keyboard-enter window-keyboard-enter
	    #:window-keyboard-leave window-keyboard-leave
	    #:update-hz 10
	    #:clear-color black
	    #:load load
	    #:update update-wrap
	    #:draw draw-wrap))
