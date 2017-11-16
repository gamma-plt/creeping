let gcd (m : int) (n : int) : int =
	if m = 0 then n
	else gcd (n % m) m
in gcd 54 24 end