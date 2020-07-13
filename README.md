# Fill-in-puzzle-solver
Automatic solver for fill-in puzzles written in Prolog

This program is built around the main predicate, puzzle_solution,
which takes an unfilled or partially filled 'Fill-in Puzzle', and 
a list of words, and fills in the puzzle consistent with the game's
rules. 

The strategy taken is to first build a list of 'slots' from the puzzle
where words are to be placed. This is done by iterating through each row
of the puzzle and using accumulators to hold the slots. The puzzle is
then transposed and the same method applied to find the vertical slots.

Once complete, each slot must correspond to a word in the word list,
so it is now simply a task of determining which word goes into which slot.
To minimise the search space, the slot with the minimum number of unifiable
words is unified at each step, to reduce the probability of backtracking.
This speeds up the process and allows puzzles of considerable size to be 
solved.
