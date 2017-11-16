let multiple (n : int) : bool = (n % 3 = 0) || (n % 5 = 0) in
	let sum (xs : [int]) : int =
		match xs with
			[] -> 0
			| x::xs -> 
				if multiple x then x + (sum xs)
				else sum xs
	in sum [1..(1000 - 1)] end
end