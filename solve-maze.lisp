;;;; Name:       Evan Hosseini
;;;; Class:      CS710 - AI
;;;; Assignment: HW #1
;;;; Date:       25 Sept 2014

;;; Application entry point solve-maze
;;; Pass in an input maze and a starting position between 1-3
(defun solve-maze (ifile ofile startpos) 
  "Program entry point"
  ;; Initialize the maze and the state counter
  (setf maze ())
  (setf state-cnt 0)
  
  ;; Set the output file name
  (setf outfile 
		(concatenate 'string ofile "_" (write-to-string startpos) ".out"))

  ;; read the passed input maze
  (setf maze (read-input-maze ifile))
  
  ;; get start pos coordinates
  (setf start-pos (start-pos-lookup ifile startpos maze))

  ;; Direction dump header
  (write-to-file "Actions taken:" t)

  ;; kick of the recursive search and report results
  (if (string= (back-track start-pos) 'success)
	  (write-to-file "Found a solution" t)
	  (write-to-file "No solution found.." t))

  ;; Dump the maze traversal
  (write-maze-to-file maze nil))

(defun back-track (currpos)
  "Recursive backtracking search function"
  ;; create an action list
  (setf actions (list #'go-left #'go-right #'go-up #'go-down))

  ;; check if we are in the desired state
  (when (string= (interpret-value currpos) 'done)
	(return-from back-track 'success))

  ;; check if the current position has already been marked as visited
  (when (string= (interpret-value currpos) 'marked) 
	(return-from back-track nil))

  ;; mark the current position as being visited
  (mark currpos)

  ;; increment the state counter
  (incf state-cnt)
  
  ;; loop over the possible actions list
  ;; for each possible action, determine if it will land you in an allowed state
  ;; for each acceptable next state, progress to the next level in the tree
  ;; if none of the four directions lead to an acceptable state, we return
  ;; nil and bounce back to the given node's parent in the recursion
  ;; Stop when we have landed on desired state or we have returned nil from
  ;; each level from the root (the entry point) in the recursion
  (loop for action in actions
  	   do (if (string/= (interpret-value (funcall action currpos t)) nil)
				(if (string= (back-track (funcall action currpos nil)) 'success)
					(return-from back-track 'success))))
  (return-from back-track nil))

(defun mark (pos)
  "Function to mark current position as having been visited"
  (setf (nth (second pos) (nth (first pos) maze)) 'O*))

(defun interpret-value (pos)
  "Function to read value from give coordinate and interpret its value"
  ;; Perform bounds checking
  (if (or 
	   (< (first pos) 0) 
	   (> (first pos) (list-length maze))
	   (< (second pos) 0)
	   (> (second pos) (list-length (first maze))))
	  (return-from interpret-value nil))
  
  ;; Get the value
  (setf val (nth (second pos) (nth (first pos) maze)))

  ;; Determine if this is a viable state and return
  (cond ((string= val "O") t)
        ((string= val "O*") 'marked)
        ((string= val "+") nil)
        ((string= val "E") 'done)))
         
(defun go-left (currpos check)
  "Helper function for go-left action"

  ;; log if we are actually moving to next sate
  (unless check (write-to-file "go-left" nil))

  ;; Move 1 space left
  (setf nextpos (list (first currpos) (- (second currpos) 1))))

(defun go-right (currpos check)
  "Helper function for go-right action"

  ;; log if we are actually moving to next sate
  (unless check (write-to-file "go-right" nil))

  ;; Move 1 space right
  (setf nextpos (list (first currpos) (+ (second currpos) 1))))

(defun go-up (currpos check)
  "Helper function for go-up action"

  ;; log if we are actually moving to next sate
  (unless check (write-to-file "go-up" nil))

  ;; Move 1 space up
  (setf nextpos (list (- (first currpos) 1) (second currpos))))

(defun go-down (currpos check)
  "Helper function for go-down action"

  ;; log if we are actually moving to next sate
  (unless check (write-to-file "go-down" nil))

  ;; Move 1 space down
  (setf nextpos (list (+ (first currpos) 1) (second currpos))))

(defun read-input-maze (filename)
  "Function to read input maze from file"
  (with-open-file (ifile filename)
	(do ((line (read-line ifile nil)    ; var init-form
			   (read-line ifile nil)))  ; step-form
		((null line))                   ; eof check
	  ;; Build up the maze contents in a string
	  (setf maze (concatenate 'string maze " " line))))
  ;; Loop through the built up string and return content as a list of lists
  (with-input-from-string (in maze)
	(loop for data = (read in nil in)
	   until (eq data in)
	   collect data)))

(defun start-pos-lookup (filename i maze)   ; i is the ith starting point
  "Function to determine one of three fixed starting positions per maze"
  (cond ((string= filename "ex-maze.dat") 
		(cond ((= i 1)
			   (setf (nth 0 (nth 1 maze)) 'O*)  ; pos 1 0 (per hw spec)
			   (write-maze-to-file maze t)
			   (setf (nth 0 (nth 1 maze)) 'O)   ; reset maze
			   (list 1 0))     ; return starting coordinates for search use
			  ((= i 2)
			   (setf (nth 0 (nth 3 maze)) 'O*)  ; pos 3 0 (per hw spec)
			   (write-maze-to-file maze t)
			   (setf (nth 0 (nth 3 maze)) 'O)   ; reset maze
			   (list 3 0))     ; return starting coordinates
			  ((= i 3)
			   (setf (nth 0 (nth 5 maze)) 'O*)  ; pos 5 0 (per hw spec)
			   (write-maze-to-file maze t)
			   (setf (nth 0 (nth 5 maze)) 'O)   ; reset maze
			   (list 5 0)))) ; return starting coordinates
		((string= filename "test1.dat") 
		 (cond ((= i 1)
				(setf (nth 0 (nth 1 maze)) 'O*)  ; pos 1 0
				(write-maze-to-file maze t)
				(setf (nth 0 (nth 1 maze)) 'O)   ; reset maze
				(list 1 0))     ; return starting coordinates for search use
			   ((= i 2)
				(setf (nth 2 (nth 7 maze)) 'O*)  ; pos 7 2
				(write-maze-to-file maze t)
				(setf (nth 2 (nth 7 maze)) 'O)   ; reset maze
				(list 7 2))     ; return starting coordinates
			   ((= i 3)
				(setf (nth 7 (nth 11 maze)) 'O*)  ; pos 11 7
				(write-maze-to-file maze t)
				(setf (nth 7 (nth 11 maze)) 'O)   ; reset maze
				(list 11 7)))))) ; return starting coordinates

(defun write-maze-to-file (maze input)
  "Helper function for writing input maze to file"
  (with-open-file (ostream outfile
						   :direction :output
						   :if-exists :append  ; append to eof
						   :if-does-not-exist :create)
	(if input
		(progn
		  (print "Input maze w/ starting location (*):")  ; print to console 
		  (print "Input maze w/ starting location (*):" ostream)) ; and file
		(progn
		  (print "Output maze w/ each visited location (*):")
		  (print "Output maze w/ each visited location (*):" ostream)))
	(loop for x in maze do   ; loop over the maze and print each sublist
		 (print x ostream)
		 (print x))
	(fresh-line ostream)
	(fresh-line ostream)))

(defun write-to-file (message blank-line)
  "Helper function for writing a message to file"
  (with-open-file (ostream outfile
						   :direction :output
						   :if-exists :append  ; append to eof
						   :if-does-not-exist :create)
	(print message)           ; print to console 
	(print message ostream)   ; and file
	(when blank-line (fresh-line ostream))))
