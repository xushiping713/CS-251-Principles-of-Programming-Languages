(* Shiping Xu *)

(* CS 251 Spring '17 Problem Set 7 Solo Problem 1 marbles *)

fun cons x ys = x :: ys (* curried list consing operator *)
						    
fun range lo hi = (* list all ints from lo up to (but not including) hi *)
  List.tabulate(hi - lo, fn i => i + lo)

(* Put any other helper functions here, *above* your marbles definition *)
fun iterate next isDone finalize state =
  if (isDone state) then
      (finalize state)
  else
      (iterate next isDone finalize (next state))

(* Define your marbles function below *)


fun marbles m c =
  case (m,c) of
      (_,1) => (m::[])::[] (* base case when there's only 1 cup *)
    | (0,_) => (foldr (fn (a,b) => cons 0 b) [] (range 0 c))::[] (* base case when 0 marbles *)
    | (_,_) => (iterate
		    (fn (n1, n2, l) =>
			(n1-1,n2+1,(map (fn x => cons n1 x) (marbles n2 (c-1)))@l))
		    (fn (n1, n2, l) =>
			((n1=0) orelse (n2=m)))
		   (fn (n1, n2, l) =>
		       (map (fn x => cons n1 x) (marbles n2 (c-1)))@l)
		   (m,0,[]))
