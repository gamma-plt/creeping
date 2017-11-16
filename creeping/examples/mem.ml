let mem (x : int) (xs : [int]) : int =
	match xs with
		[] -> []
		| h::xs -> 
			if h = x then true
			else mem x
in mem x end