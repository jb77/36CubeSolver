
valid([]).
valid([Head|Tail]) :- 
    fd_all_different(Head), 
    valid(Tail).


coloring(Puzzle, Solution) :-
   Solution = Puzzle,
   Puzzle = [A1, A2, A3, A4, A5, A6,
             B1, B2, B3, B4, B5, B6,
             C1, C2, C3, C4, C5, C6,
             D1, D2, D3, D4, D5, D6,
             E1, E2, E3, E4, E5, E6,
             F1, F2, F3, F4, F5, F6], 
                  
  fd_domain(Puzzle, 1, 6), 

  RowA = [A1, A2, A3, A4, A5, A6],
  RowB = [B1, B2, B3, B4, B5, B6],
  RowC = [C1, C2, C3, C4, C5, C6],
  RowD = [D1, D2, D3, D4, D5, D6],
  RowE = [E1, E2, E3, E4, E5, E6],
  RowF = [F1, F2, F3, F4, F5, F6],

  Col1 = [A1,B1,C1,D1,E1,F1],
  Col2 = [A2,B2,C2,D2,E2,F2],
  Col3 = [A3,B3,C3,D3,E3,F3],
  Col4 = [A4,B4,C4,D4,E4,F4],
  Col5 = [A5,B5,C5,D5,E5,F5],
  Col6 = [A6,B6,C6,D6,E6,F6],

  Ones = [A1,B5,C2,E6,F4],
  Twos = [A5,B1,C4,D6,E2,F3],
  Threes = [A2,B6,C3,D4,E5,F1],
  Fours = [A3,B4,C6,D2,E1,F5],
  Fives = [A4,B2,C5,D1,E3,F6],
  Zeros = [A6,C1,D5,E4,F2],


  valid([RowA, RowB, RowC,RowD, RowE, RowF,Col1, Col2, Col3, Col4, Col5, Col6, Ones, Twos, Threes, Fours, Fives, Zeros]),
  fd_labeling(Puzzle).








  





  
  