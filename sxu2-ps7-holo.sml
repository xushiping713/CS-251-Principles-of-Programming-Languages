fun range lo hi =
  if hi <= lo then
      []
  else
      lo :: (range (lo+1) hi)

fun digitsToDecimal digits =
  foldl (fn (digit, sum) => sum*10+digit) 0 digits
	      
fun cartesianProduct xs ys =
  foldr (fn (x, subres) => (map (fn y => (x,y)) ys) @ subres) [] xs
		     
fun partition pred xs =
  foldr (fn (x, (fst,scd)) => if (pred x) then
				    (x::fst, scd)
		             else
			            (fst, x::scd)
	) ([],[]) xs

fun doAllContainMultiple m nss =
  List.all (fn (ns) => List.exists
			   (fn (n) => (n mod m)=0
			   ) ns
	   ) nss
	   
fun keepBiggerThanNext [] = []
  | keepBiggerThanNext (nums as (fst::rst)) = map (fn (a,b) => a)
					 (List.filter (fn (car,cdr) => car>cdr)
					             (ListPair.zip (nums,rst)))

fun foldrTernop ternop nullval [] = nullval
  | foldrTernop ternop nullval (x::xs) = (ternop (x, xs, (foldrTernop ternop nullval xs)))

						
fun keepBiggerThanSomeFollowing nums =
  foldrTernop (fn (fstNum,rstNums,bigs) =>
		 if (List.exists (fn n => fstNum>n) rstNums) then
		    fstNum::bigs
		 else
		    bigs) [] nums   

fun genlist next isDone isKeepDoneValue seed =
  if (isDone seed) then
      if isKeepDoneValue then
	  [seed]
      else
	  []
  else
      seed :: (genlist next isDone isKeepDoneValue (next seed))
    
fun partialSumsTable ns =
  genlist (fn (nums as (fst::rst), ans) => (rst,fst+ans))
	  (fn (nums,ans) =>
	      case nums of
		  [] => true
	       | nums  => false)			      
	  true
	  (ns,0)

fun iterate next isDone finalize state =
  if (isDone state) then
      (finalize state)
  else
      (iterate next isDone finalize (next state))

fun fibPairs threshold =
  iterate (fn (a,b,pairs) => (b,a+b,(a,b)::pairs))
	  (fn (a,b,pairs) => b>=threshold)
	  (fn (a,b,pairs) => List.rev ((a,b)::pairs))
	  (0,1,[])
