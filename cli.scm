(define-module (g2048 cli)

;;require-module (ice-9 format)
;;usage -> [~4d] <in> print-board
;;
  #:use-module (ice-9 format)

;;require-module (srfi srfi-64)
;;usage -> test-assert
;;
  #:use-module (srfi srfi-64)

  #:export (play))

;;prodedure-name 2or4-generator
;;input -> <$:nil>
;;output -> _ (number)
;;note -> "it only generates 2 or 4, with a 10% of outputting 4"
;;
(define 2or4-generator
  (lambda ()
    (let ([random-number (random 10)])
      (if (zero? random-number)
	  4
	  2))))

;;procedure-name print-board
;;input -> board (array::4*4)
;;output -> <$:nil>
;;
(define print-board
  (lambda (board)
    (do ((i 0 (1+ i)))
	((> i 3))
      (do ((j 0 (1+ j)))
	  ((> j 3))
	(format #t "[~4d]" (array-ref board i j)))
      (newline))))

;;procedure-name spawn!
;;input -> 4x4board (array::4*4)
;;output -> <$:nil>
;;
(define spawn!
  (lambda (4x4board)
    (let ([indexes '()])
      (do ((i 0 (1+ i)))
	  ((> i 3))
	(do ((j 0 (1+ j)))
	    ((> j 3))
	  (when (zero? (array-ref 4x4board i j))
	    (set! indexes (cons (cons i j) indexes)))))
      (let* ([indexes-length (length indexes)]
	     [used-index-index (random indexes-length)]
	     [used-index-pair (list-ref indexes used-index-index)])
	(array-set! 4x4board (2or4-generator) (car used-index-pair) (cdr used-index-pair))))))

;;procedure-name update-row!
;;input -> row (array::1*4)
;;output -> <$:nil>
;;
(define update-row!
  (lambda (row)
    (unless (array-equal? row #(0 0 0 0))
      (let ([lst '()] [l 0])
	(when (not (zero? (array-ref row 3))) (set! l (1+ l)) (set! lst (cons (array-ref row 3) lst)))
	(when (not (zero? (array-ref row 2))) (set! l (1+ l)) (set! lst (cons (array-ref row 2) lst)))
	(when (not (zero? (array-ref row 1))) (set! l (1+ l)) (set! lst (cons (array-ref row 1) lst)))
	(when (not (zero? (array-ref row 0))) (set! l (1+ l)) (set! lst (cons (array-ref row 0) lst)))
        (do ((i 0 (1+ i)))
	    ((>= i (- 4 l)))
	  (array-set! row 0 i))
	(do ((i (- 4 l) (1+ i)) (j 0 (1+ j)))
	    ((>= i 4))
	  (array-set! row (list-ref lst j) i)))
      (if (= (array-ref row 3) (array-ref row 2))
	  (begin (array-set! row (+ (array-ref row 3) (array-ref row 2)) 3)
		 (array-set! row (array-ref row 1) 2)
		 (array-set! row (array-ref row 0) 1)
		 (array-set! row 0 0)
		 (when (= (array-ref row 2) (array-ref row 1))
		   (array-set! row (+ (array-ref row 2) (array-ref row 1)) 2)
		   (array-set! row 0 1)))
	  (if (= (array-ref row 2) (array-ref row 1))
	      (begin (array-set! row (+ (array-ref row 2) (array-ref row 1)) 2)
		     (array-set! row (array-ref row 0) 1)
		     (array-set! row 0 0))
	      (when (= (array-ref row 1) (array-ref row 0))
		(array-set! row (+ (array-ref row 1) (array-ref row 0)) 1)
		(array-set! row 0 0)))))))

;;procedure-name update-board!
;;input -> board (array::4*4)
;;output -> <$:nil>
;;note -> "this procedure can assume that the opertion is shifting the piles right"
;;
(define update-board!
  (lambda (board)
    (do ((i 0 (1+ i)))
	((>= i 4))
     (update-row! (array-cell-ref board i)))))

;;procedure-name row-can-shift-right?
;;input -> row (array::1*4)
;;output -> _ (#t or #f)
;;
(define row-can-shift-right?
  (lambda (row)
    (if (and (zero? (array-ref row 0))
	     (zero? (array-ref row 1))
	     (zero? (array-ref row 2)))
	#f
	(if (or (zero? (array-ref row 3))
		(and (not (zero? (array-ref row 0))) (member 0 (list
								(array-ref row 1)
								(array-ref row 2)
								(array-ref row 3))))
		(and (not (zero? (array-ref row 1))) (member 0 (list
								(array-ref row 2)
								(array-ref row 3))))
		(and (not (zero? (array-ref row 2))) (member 0 (list
								(array-ref row 3)))))
	     #t
	     (if (or (and (not (zero? (array-ref row 0))) (= (array-ref row 0) (array-ref row 1)))
		     (and (not (zero? (array-ref row 1))) (= (array-ref row 1) (array-ref row 2)))
		     (and (not (zero? (array-ref row 2))) (= (array-ref row 2) (array-ref row 3))))
		 #t
		 #f)))))

(define test-row-can-shift-right?
  (lambda ()
    (test-begin "row-can-shift-right? test")
    (test-assert (row-can-shift-right? #(2 0 0 0)))
    (test-assert (row-can-shift-right? #(0 2 0 0)))
    (test-assert (row-can-shift-right? #(0 0 2 0)))
    (test-assert (not (row-can-shift-right? #(0 0 0 2))))
    (test-assert (row-can-shift-right? #(4 2 0 0)))
    (test-assert (row-can-shift-right? #(2 0 0 2)))
    (test-assert (row-can-shift-right? #(1024 8 2 2)))
    (test-end "row-can-shift-right? test")))

;;procedure-name can-shift-right?
;;input -> board (array::4*4)
;;output -> _ (#t or #f)
;;
(define can-shift-right?
  (lambda (board)
    (or (row-can-shift-right? (array-cell-ref board 0))
	(row-can-shift-right? (array-cell-ref board 1))
	(row-can-shift-right? (array-cell-ref board 2))
	(row-can-shift-right? (array-cell-ref board 3)))))

(define test-can-shift-right?
  (lambda ()
    (test-begin "can-shift-right? test")
    (test-assert (can-shift-right? #2((2 0 0 2)
				      (0 0 0 0)
				      (0 0 0 0)
				      (0 0 0 0))))
    (test-assert (can-shift-right? #2((2 0 0 0)
				      (0 0 0 0)
				      (0 0 0 0)
				      (2 0 0 0))))
    (test-assert (not (can-shift-right? #2((2 8 4 8)
					   (2 16 4 8)
					   (0 0 0 4)
					   (0 0 0 2)))))
    (test-end "can-shift-right? test")))

;;procedure-name movable?
;;input -> board (array::4*4)
;;output -> _ (symbol)
;;
(define movable?
  (lambda (board)
    (let* ([rows board]
	   [columns (make-shared-array rows
				       (lambda (i j)
					 (list j i))
				       4 4)]
	   [rows-reversed (make-shared-array rows
					     (lambda (i j)
					       (list i (- 3 j)))
					     4 4)]
	   [columns-reversed (make-shared-array columns
						(lambda (i j)
						  (list i (- 3 j)))
						4 4)])
      (or (can-shift-right? rows)
	  (can-shift-right? rows-reversed) 
	  (can-shift-right? columns)
	  (can-shift-right? columns-reversed)))))

(define test-movable?
  (lambda ()
    (test-begin "movable? test")
    (test-assert (not (movable? #2((8 4 2 16)
				   (2 8 4 32)
				   (4 2 8 16)
				   (128 4 16 64)))))
    (test-assert (movable? #2((0 0 0 0)
			      (0 2 0 0)
			      (0 0 0 0)
			      (0 0 0 2))))
    (test-end "movable? test")))

;;prodedure-name check-game-status!
;;input -> board (array::4*4)
;;output -> _ (symbol)
;;
(define check-game-status
  (lambda (board)
    (let ([all-tiles (list (array-ref board 0 0)
			   (array-ref board 0 1)
			   (array-ref board 0 2)
			   (array-ref board 0 3)
			   (array-ref board 1 0)
			   (array-ref board 1 1)
			   (array-ref board 1 2)
			   (array-ref board 1 3)
			   (array-ref board 2 0)
			   (array-ref board 2 1)
			   (array-ref board 2 2)
			   (array-ref board 2 3)
			   (array-ref board 3 0)
			   (array-ref board 3 1)
			   (array-ref board 3 2)
			   (array-ref board 3 3))])
      (if (member 2048 all-tiles)
	  'win
	  (if (movable? board)
	      'continue
	      'gameover))))

;;procedure-name play
;;input -> <$:nil>
;;output -> _ (array:4*4)
;;
(define play
  (let* ((turn #f)
	 (board #f)
	 (rows #f)
	 (columns #f)
	 (rows-reversed #f)
	 (columns-reversed #f)
	 (available-moves #f)
	 (refresh! (lambda ()
		     (set! turn 1)
		     (set! board (make-array 0 4 4))
		     (set! rows (make-shared-array board
						   (lambda (i j)
						     (list i j))
						   4 4))
		     (set! columns (make-shared-array rows
						      (lambda (i j)
							(list j i))
						      4 4))
		     (set! rows-reversed (make-shared-array rows
							    (lambda (i j)
							      (list i (- 3 j)))
							    4 4))
		     (set! columns-reversed (make-shared-array columns
							       (lambda (i j)
								 (list i (- 3 j)))
							       4 4))
		     (set! available-moves (movable? rows)))))
    (refresh!)
    (spawn! rows)
    (lambda* (#:optional move #:key refresh? (spawn? #t))
      (if refresh?
	  (refresh!)
          (begin (set! turn (1+ turn))
		 (case move
		   [(up) (update-board! columns-reversed)]
		   [(down) (update-board! columns)]
		   [(left) (update-board! rows-reversed)]
		   [(right) (update-board! rows)])))
      (when spawn? (spawn! rows))
      `((turn . ,turn)
	(board . ,board)
	(game-status . ,(check-game-status rows))))))
