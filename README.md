backtracking-search
===================

CS710 HW#1
Evan Hosseini
4 Oct 2014

Overview:
This program implements a simple backtracking search algorithm.  There are 
three input mazes included in this source, ex-maze.out, test1.out, and 
test2.out.  For each of these three mazes, three different starting coordinates
have been embedded into the main application of the program, solve-maze.lisp.
The solve-maze function takes three parameters.  The first is a string of the 
filename (in the current directory), a base name to write the output to, and an
integer 1-3 specifying the starting coordinate to load.  All three starting 
points in all three mazes are exercised in the driver source, hw_driver.lisp.
Each starting point of each maze will generate its own output file.  The naming
convention of the output files are the base file name passed to the solve-maze
function, plus "_startPos" where startPos is the integer starting value passed
to solve-maze as well.

Deviations:
The program doesn't log directions taken in backtracking, only the steps taken
towards new states.  This is due to the way the algorithm was implemented.  Each
recursive call to the back tracking algorithm traverses further down the tree
structure and when a next move is not available, we simply return back to a
higher level in the tree, without explicitly making a call to one of the four
direction routines (go-left, go-right, go-up, go-down).  However, I have 
included in the program a dump after the maze is solved of all the states which
have been visited so it is very easy to observe the algorithm's traversal 
through the maze.

Instruction for use:
Unzip the content of CS710_HW1_EvanHosseini.zip
Execute the following command:
clisp hw_driver.lisp

Observe all of the generated output files as described above, or run some
command like "cat *.out" on a *nix system or something similar on windows to 
dump all of the output files to the terminal at once.
