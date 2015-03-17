-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
-- Moves the discs from the first to the third peg 
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a, c)] ++ hanoi (n-1) b a 
