;;;; Name:       Evan Hosseini
;;;; Class:      CS710 - AI
;;;; Assignment: HW #1
;;;; Date:       25 Sept 2014

;;; Driver for homework

;; Load the main program
(load "solve-maze.lisp")

;; Start by solving the example maze
(solve-maze "ex-maze.dat" "ex-maze" 1)  ;; solvable
(solve-maze "ex-maze.dat" "ex-maze" 2)  ;; unsolvable
(solve-maze "ex-maze.dat" "ex-maze" 3)  ;; unsolvable

;; Next, solve the 14x14 maze
(solve-maze "test1.dat" "test1" 1)  ;; solvable
(solve-maze "test1.dat" "test1" 2)  ;; unsolvable
(solve-maze "test1.dat" "test1" 3)  ;; solvable, no backtracking needed

;; Finally, solve the 20x20 maze
(solve-maze "test2.dat" "test2" 1)  ;; unsolvable
(solve-maze "test2.dat" "test2" 2)  ;; solvable, no backtracking needed
(solve-maze "test2.dat" "test2" 3)  ;; solvable
