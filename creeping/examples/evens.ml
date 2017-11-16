let even (n : int) : bool = n % 2 = 0 in
	let evens (xs : [int]) : [int] =
		match xs with
			[] -> []
			| x::xs -> 
				if even x then (x::evens xs)
				else evens xs
 	in evens [1..1000] end
end