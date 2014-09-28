;;;; Name:       Evan Hosseini
;;;; Class:      CS710 - AI
;;;; Assignment: HW #1
;;;; Date:       25 Sept 2014

;;; Application entry point solve-maze
;;; Pass in an input maze and a starting position between 1-3
(defun solve-maze (filename startpos) 
  "Program entry point"
  (setf maze ())

  ;; read the passed input maze
  (setf maze (read-input-maze filename))
  
  ;; get start pos coordinates
  (setf start-pos (start-pos-lookup filename startpos maze)))

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
        ((string= val "O*") t)
        ((string= val "+") nil)
        ((string= val "E") 'done)))
         
(defun go-left (currpos)
  "Helper function for go-left action"
  ;; Move 1 space left
  (setf nextpos (list (first currpos) (- (second currpos) 1))))

(defun go-right (currpos)
  "Helper function for go-right action"
  ;; Move 1 space right
  (setf nextpos (list (first currpos) (+ (second currpos) 1))))

(defun go-up (currpos)
  "Helper function for go-up action"
  ;; Move 1 space up
  (setf nextpos (list (- (first currpos) 1) (second currpos))))

(defun go-down (currpos)
  "Helper function for go-down action"
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
			   (write-output "ex-maze-out.dat" maze)
			   (list 1 0))     ; return starting coordinates for search use
			  ((= i 2)
			   (setf (nth 0 (nth 3 maze)) 'O*)  ; pos 3 0 (per hw spec)
			   (write-output "ex-maze-out.dat" maze)
			   (list 3 0))     ; return starting coordinates
			  ((= i 3)
			   (setf (nth 0 (nth 5 maze)) 'O*)  ; pos 5 0 (per hw spec)
			   (write-output "ex-maze-out.dat" maze)
			   (list 5 0)))))) ; return starting coordinates

(defun write-output (outfile maze)
  "Helper function for writing output to file"
  (with-open-file (ostream outfile
						   :direction :output
						   :if-exists :append  ; append to eof to build up rpt
						   :if-does-not-exist :create)
	(print "Input maze w/ starting location (*):")  ; print to console 
	(print "Input maze w/ starting location (*):" ostream) ; and file
	(loop for x in maze do   ; loop over the maze and print each sublist
		 (print x ostream)
		 (print x))
	(fresh-line ostream)))
