% Author: Conor Rogers

% Purpose: Solver for fill-in puzzles

:- ensure_loaded(library(clpfd)).

% This program is built around the main predicate, puzzle_solution,
% which takes an unfilled or partially filled 'Fill-in Puzzle', and 
% a list of words, and fills in the puzzle consistent with the game's
% rules. 
%
% The strategy taken is to first build a list of 'slots' from the puzzle
% where words are to be placed. This is done by iterating through each row
% of the puzzle and using accumulators to hold the slots. The puzzle is
% then transposed and the same method applied to find the vertical slots.
%
% Once complete, each slot must correspond to a word in the word list,
% so it is now simply a task of determining which word goes into which slot.
% To minimise the search space, the slot with the minimum number of unifiable
% words is unified at each step, to reduce the probability of backtracking.
% This speeds up the process and allows puzzles of considerable size to be 
% solved.

% The main predicate. First, the list of slots are generated, then unifier
% handles the unification of each slot with a word in the most efficient
% manner.
puzzle_solution(Puzzle, WordList) :-
    slotPuzzle(Puzzle, Slots),
    unifier(Slots, WordList).

%%%%%%%%%%%%%%%%%%%%%%%%%
% SLOT-FINDING PREDICATES
%%%%%%%%%%%%%%%%%%%%%%%%%

% Slots in each row of the puzzle are found, then after transposing the puzzle,
% the same function can be run to find the slots in each column.
slotPuzzle(Puzzle, Slots) :- 
    slotRows(Puzzle, RowSlots),
    transpose(Puzzle, PuzzleT),
    slotRows(PuzzleT, ColSlots),
    append(RowSlots, ColSlots, Slots).

% Each row is considered seperately, its slots appended to the accumulator,
% before recursively calling again on the rest of the rows.
slotRows(Rows, Slots) :- slotRows(Rows, [], Slots).
slotRows([], Slots, Slots).
slotRows([Row|Rest], Acc, Slots) :-
    slotRow(Row, RowSlots),
    append(Acc, RowSlots, Acc1),
    slotRows(Rest, Acc1, Slots).
 
% Returns a list of valid slots within a row.
% It calls a helper predicate slotRow/4.
% The second argument accumulates an incomplete slot, while the third argument
% accumulates the list of already completed slots for that row.
slotRow(Row, Slots) :- slotRow(Row, [], [], Slots).

% Base case, if the current slot Acc is a valid slot, add it to the list of 
% slots SlotAcc, and return result.
% Otherwise, the list of slots SlotAcc is the result.
slotRow([], Acc, SlotAcc, Slots):- 
    ( length(Acc, N), N > 1
    -> append(SlotAcc, [Acc], Slots)
    ; Slots = SlotAcc ).

% If the head element is '#', it checks Acc to see if it is a valid slot, 
% and adds it to SlotAcc. Acc is reset to [], and slotRow is called again 
% on the rest of the list.
% Otherwise, it appends the current head to Acc, and calls slotRow
% on the rest of the list. 
slotRow([Head|Tail], Acc, SlotAcc, Slot):-
    ( ground(Head), Head = '#'
    -> ( length(Acc, N), N > 1
       ->  append(SlotAcc, [Acc], SlotAcc1),
           Acc1 = [],
           slotRow(Tail, Acc1, SlotAcc1, Slot)
       ;   Acc1 = [],
           slotRow(Tail, Acc1, SlotAcc, Slot) )
    ; append(Acc, [Head], Acc1), 
      slotRow(Tail, Acc1, SlotAcc, Slot) ).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%
% SLOT UNIFYING PREDICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%

% Takes a list of slots, and the word list. slotNumList returns the list of
% slots with the number of words they could be unified with. This is then
% sorted in ascending order, and restructured to contain only the slots.
% The first slot (the one with the least unifiable words) is then unified
% with a word in the word list. Unifier is then recursively called on the
% rest of the slots, with the rest of the words from the word list. 
unifier([], []).
unifier(Slots, WordList) :-
    slotNumList(Slots, WordList, List),
    msort(List, ListSorted),
    slotsOnly(ListSorted, [Slot|RestSlots]),
    select(Slot, WordList, ReducedList),
    unifier(RestSlots, ReducedList).

% First, numWords finds the number of words potentially unifiable with the 
% first slot in the list. This number, and the slot itself, is appended to the 
% accumulator, before recursively calling on the rest of the slots. 
slotNumList(Slots, WordList, List) :- 
    slotNumList(Slots, WordList, [], List).
slotNumList([], WordList, List, List). 
slotNumList([Slot|Rest], WordList, Acc, List) :-
    numWords(Slot, WordList, Num),
    append(Acc, [[Num, Slot]], Acc1),
    slotNumList(Rest, WordList, Acc1, List).

% numWords works through each word in the word list, and uses an accumulator
% to keep count of the words which could be unifiable with the Slot.
numWords(Slot, WordList, Num) :- numWords(WordList, Slot, 0, Num).
numWords([], Slot, Num, Num).
numWords([Word|Rest], Slot, Acc, Num) :-
    ( \+ \+ Slot = Word
    -> Acc1 is Acc + 1, numWords(Rest, Slot, Acc1, Num)
    ; numWords(Rest, Slot, Acc, Num) ).

% slotsOnly is used within the unifier predicate to turn the list of slots 
% and their number of unifiable words back into a list of just slots.
slotsOnly(List, Slots) :- slotsOnly(List, [], Slots).
slotsOnly([], Slots, Slots).
slotsOnly([[_,Slot]|Rest], Acc, Slots) :-
    append(Acc, [Slot], Acc1),
    slotsOnly(Rest, Acc1, Slots).
