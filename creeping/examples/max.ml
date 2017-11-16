let max (x:char) (y:char) : char = if x > y then x else y in
	let max3 (xyz : (char * char * char)) : char = 
		let x, y, z = #1(xyz), #2(xyz), #3(xyz) 
		in max #1(xyz) (max #2(xyz) z) end
	in max3 ('a', 'b', 'c') end
end