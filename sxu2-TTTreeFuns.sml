(* This is the file TTTreeFuns.sml *)

(* Here, write your definition of elts: TTTree -> int list *)
fun elts L = []
  | elts (W(l,X,r)) = (elts l) @ [X] @ (elts r)
  | elts (H(l,X,m,Y,r)) = (elts l) @ [X] @ (elts m) @ [Y] @ (elts r)
  
  
(* Here, write your definition of isSorted: int list -> bool *)
fun isSorted [] = true
  | isSorted (nums as (fst::rst)) =
    List.all (fn (a,b) => a<=b) (ListPair.zip (nums,rst))
	     
(* Uncomment this definition when elts and isSorted are defined *)
 
fun satisfiesOrderingProperty t = isSorted(elts t)


(* Uncomment and flesh out this skeleton of the function height: TTTree -> int option *)

(* height: TTTree -> int option *)

fun height L = SOME(0)
  | height (W(l, _, r)) = 
    (case (height(l), height(r)) of
	 (NONE,_) => NONE
       | (_,NONE) => NONE
       | _ => (if height(l)=height(r) then
		  SOME(1+ (valOf (height(l))))
	      else
		  NONE))
  | height (H(l,_,m,_,r)) =
    (case (height(l),height(m),height(r)) of
	 (NONE,_,_) => NONE
       | (_,NONE,_) => NONE
       | (_,_,NONE) => NONE
       | _  => (if (height(l)=height(m) andalso height(m)=height(r)) then
		    SOME(1+(valOf (height(l))))
		else
		    NONE))		  

(* Uncomment this definition when height is defined *)
 
fun satisfiesHeightProperty t = Option.isSome (height t)

(* Uncomment this definition when satisfiesOrderingProperty and
   satisfiesHeightProperty are defined *)
 
fun isValid t = satisfiesOrderingProperty t andalso satisfiesHeightProperty t

(* datatype for insertion *)
datatype InsResult =
	 Tree of TTTree
	 | KickedUp of TTTree * int * TTTree (* pseudo two-node "kicked up" from below *)

(* Uncomment and flesh out this skeleton of the functions insert and ins *)

					  
(* insert: int -> TTTree -> TTTree *)
fun insert v t =
  case ins v t of
      Tree t => t
    | KickedUp(l, w, r) => W(l, w, r)
and (* "and" glues together mutually recursive functions *)
(* ins: int -> TTTree -> InsResult *)
    ins v L = KickedUp (L, v, L)
  | ins v (W(l, X, r)) =
    if v <= X
    then (case ins v l of
                Tree l' => Tree(W(l', X, r))
              | KickedUp (l', w, m)  => Tree(H(l', w, m, X, r)))
    else
	(case ins v r of
	     Tree r' => Tree(W(l, X, r'))
	   | KickedUp (m, w, r') => Tree(H(l, X, m, w, r')))
  | ins v (H(l, X, m, Y, r)) =
    if v <= X
    then (case ins v l of
	      Tree l' => Tree(H(l', X, m, Y, r))
	    | KickedUp (l',w,b)  => KickedUp(W(l', w, b), X, W(m, Y, r)))
    else
	if (X<v andalso v<=Y)
	then (case ins v m of
		  Tree m' => Tree(H(l, X, m', Y, r))
		| KickedUp(b,w,c) => KickedUp(W(l, X,b), w, W(c, Y, r)))
	else
	    (case ins v r of
		 Tree r' => Tree(H(l, X, m, Y, r'))
	       | KickedUp(c,w,r') => KickedUp(W(l, X, m), Y, W(c, w, r')))
		      


(* Uncomment the following testing functions after insert and ins are defined *)

fun listToTTTree xs =
  foldl (fn (x,t) => insert x t) L xs

fun range lo hi =
  if lo >= hi then [] else lo :: (range (lo + 1) hi)

(* Return list that has all ints in nums, but those not divisible by 3
   come before those divisible by three *)
fun arrangeMod3 nums =
  let val (zeroMod3, notZeroMod3) =
	  List.partition (fn n => (n mod 3) = 0) nums
  in notZeroMod3 @ zeroMod3
  end
      
(* Make a 2-3 tree with elements from 1 up to and including size. 
   Use arrangeMod3 to mix up numbers and lead to more 3-nodes
   than we'd get with sorted integers lists *)
fun makeTTTree size =
  listToTTTree (arrangeMod3 (range 1 (size + 1)))

fun testInsert upToSize =
  let val pairs = map (fn n => (n, makeTTTree n)) (range 0 (upToSize + 1))
      val (validPairs, invalidPairs) = List.partition (fn (_,t) => isValid t) pairs
      val wrongElts = List.filter (fn (n,t) => (elts t) <> (range 1 (n + 1))) validPairs
  in if (null invalidPairs) andalso (null wrongElts) 
     then (print "Passed all test cases\n"; [])
     else if (not (null invalidPairs))
          then (print "There are invalid trees in the following cases\n"; invalidPairs)
          else (print "The elements or element order is wrong in the following cases\n"; wrongElts)
  end

				

  
	

	
  
	
	
					
			
	   
				    



						     

