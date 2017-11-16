let fact (n : int) (acc : int) : int =
	if n = 0 then acc
	else fact (n - 1) (n * acc)
in fact 5 1 end