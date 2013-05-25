-- Author: Yvette (I-Ting) Tsai
-- Email: ytsai@bu.edu
-- Date: Oct 01, 2012

-- prefix n xs
-- Description: it accepts a positive integer n and a list xs as input, 
--	and returns a list containing only the first n elements in the 
--	input list
-- prefix :: Int -> [a] -> [a]
prefix n xs
  | xs == []	= []
	| n == 0	= []
	| otherwise	= myHead(xs) : prefix (n-1) (myTail(xs))
	where
		myHead (m:ms) = m
		myTail (m:ms) = ms

	
-- suffix n xs
-- Description: it accepts a positive integer n and a list xs as input, 
--	and returns the list of elements that remain after the first n 
--	elements are dropped from the front of the list
-- suffix :: Int -> [a] -> [a]
suffix n xs
	| xs == []	= []
	| n == 0	= xs
	| otherwise = suffix (n-1) (myTail(xs))
	where
		myTail (m:ms) = ms

	
-- spile n y xs
-- Description: it takes a positive integer n, an element y, 
-- 	and a list xs. The function inserts the specified element 
-- 	y after every n elements in the list
-- split :: Int -> a -> [a] -> [a]
split n y xs
	| myLen(p) < n		= p 
	| s == []			= p ++ [y]
	| otherwise			= p ++ [y] ++ (split n y s)
	where
		p = prefix n xs
		s = suffix n xs
		myLen ms
			| ms == []	= 0
			| otherwise = 1 + myLen (suffix 1 ms)
		

-- plane r
-- Description: it takes a single integer argument r and returns 
--	the list of all points on the cartesian plane of the form 
--	(x/r, y/r) where x and y are integers, x/r is between 
--	−2 and 1, and y/r is between −1 and 1 
plane r = [(x/r, y/r) | y <- [-r .. r], x <- [-2*r .. r] ]


-- orbit (x,y)
-- Description: it takes a single point (x,y) as an argument 
--	and returns an infinite list corresponding to O(x, y). 
-- orbit :: Point( -> [point]
orbit (x,y) = orbitHelper
	where
		orbitHelper = (0,0) : (map pxy orbitHelper)
			where 
				pxy (u,v) = (u^2 - v^2 + x, 2*u*v + y)


-- disp d l
-- Description: takes two arguments: a number d and a list of pairs.
--	It returns the character from the list that corresponds to 
--	the smallest number on the list that is greater than the 
--	input d, and if d is larger than all the number in the list, 
--	disp should return a space character, ’ ’
disp :: Double -> [(Double, Char)] -> Char
disp d l
	| length(l) == 0	= ' '
	| d < fst(x)		= snd(x)
	| otherwise			= disp d (tail(l))
		where
			x = head(l)

-- mandelbrot r i l
-- Description: it takes three arguments: r represents the resolution
--	of the approximation, i represents the index of the elements 
--	to check in the orbit lists of the points, and l represents 
--	the formatting list . It returns a list of characters that 
--	corresponds to a picture approximating the shape of the 
--	Mandelbrot set on the plane.
mandelbrot r i l = split (3*r+1) '\n' (mandelHelper myPlane (length(myPlane)))
	where
		distance(x1,y1) = x1*x1 + y1*y1
		myPlane = plane r
		mandelHelper xs len
			| len == 0		= []
			| otherwise		= [(disp (distance(p)) l)] ++ (mandelHelper (tail(xs)) (len-1))
				where
					orbitList = orbit(head(xs))
					p = orbitList !! i
