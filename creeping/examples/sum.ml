let foo (n : int) (acc : int) : int =
	if n = 0 then acc
	else foo (n - 1) (n + acc)
in foo 10 0 end 