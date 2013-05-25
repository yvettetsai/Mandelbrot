-- Author: Yvette (I-Ting) Tsai
-- Email: ytsai@bu.edu
-- Date: Oct 01, 2012

-- Problem 1-a
-- prefix :: Int -> [a] -> [a]
prefix n xs
  | xs == []	= []
	| n == 0	= []
	| otherwise	= myHead(xs) : prefix (n-1) (myTail(xs))
	where
		myHead (m:ms) = m
		myTail (m:ms) = ms

	
-- Problem 1-b
-- suffix :: Int -> [a] -> [a]
suffix n xs
	| xs == []	= []
	| n == 0	= xs
	| otherwise = suffix (n-1) (myTail(xs))
	where
		myTail (m:ms) = ms

	
-- Problem 1-c
--split :: Int -> a -> [a] -> [a]
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
		

-- Problem 2-a
plane r = [(x/r, y/r) | y <- [-r .. r], x <- [-2*r .. r] ]


-- Problem 2-b
-- data Point(Double, Double) = Point (Double, Double)
-- orbit :: Point( -> [point]
orbit (x,y) = orbitHelper
	where
		orbitHelper = (0,0) : (map pxy orbitHelper)
			where 
				pxy (u,v) = (u^2 - v^2 + x, 2*u*v + y)


-- Problem 2-c
disp :: Double -> [(Double, Char)] -> Char
disp d l
	| length(l) == 0	= ' '
	| d < fst(x)		= snd(x)
	| otherwise			= disp d (tail(l))
		where
			x = head(l)

-- Problem 2-d
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
