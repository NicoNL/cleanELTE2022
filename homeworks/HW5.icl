module HW5
import StdEnv
// Solve at least two of the following tasks:

/*1. Consider the following example:
"aaaabbbcaddd" -> [(4, 'a'), (3, 'b'), (1, 'c'), (1, 'a'), (3, 'd')]
i.e. consecutive duplicates are grouped together and their count is taken in a tuple
together with the character.
This method is called Run-length encoding.
Your task is to implement "encode" function which does this.
*/

encode :: String -> [(Int,Char)]
encode s = f1 x
where x = (fromString s)
	  f1 [] = []
      f1 [x] = [(1,x)] 
      f1 [x:xs] = [((length(takeWhile ((==)x) [x:xs])),x)] ++ f1 (drop (length(takeWhile ((==)x) [x:xs])) [x:xs])

	  
//Start = encode "aaaabbbcaddd" // [(4, 'a'), (3, 'b'), (1, 'c'), (1, 'a'), (3, 'd')]
//Start = encode "" // []
//Start = encode "aa" // [(2, 'a')]
//Start = encode "abcde" // [(1, 'a'), (1, 'b'), (1, 'c'), (1, 'd'), (1, 'e')]


/*2. Given the list of tuples. Each tuple has 3 element: L, R and Step.
For each tuple generate a list of numbers from L to R increasing with Step (L,L+Step,L+2*Step...).
For example, if L is 1, R is 10 and step is 4 list would be [1,5,9]. Your function should return
a list of lists.
*/


expandList :: [(Int,Int,Int)] -> [[Int]]
expandList x = map tupletolist x
where tupletolist (n,m,s)
      | m < n = [ x \\ x <- take (length [n,(n-1)..m]) (iterate ((+)(s)) n)| isMember x [n,(n-1)..m] && s < 0]
	  | m > n = [ x \\ x <- take (length [n..m]) (iterate ((+)s) n)| m >= x]	
		

//Start = expandList [(1,10,4), (3,5,4), (5,4,1), (1,10,3)] // [[1,5,9],[3],[],[1,4,7,10]]
//Start = expandList [] // []
//Start = expandList [(5,3,-1),(2,13,3),(1,8,1)] // [[5,4,3],[2,5,8,11],[1,2,3,4,5,6,7,8]]
//Start = expandList [(1,12,100), (2,5,10), (4,-1,-10)] // [[1],[2],[4]]


/*3. Write a function that will do a circle rotation of the numbers in a list of tuples.
For example: rotate 1 [(1,2),(3,4),(5,6)] will give you [(2,3),(4,5),(6,1)]
*/
ltll :: [(Int,Int)] -> [[Int]]
ltll x = [fst (unzip x)] ++ [snd (unzip x)]

//Start = ltll [(1,2),(3,4),(5,6)]

round :: [[Int]] -> [[Int]]
round [[x,y:xs],[z,w:zs]] =  [[z,w:zs]] ++ [[y:xs]++[x]]

r :: [(Int,Int)] -> [(Int,Int)]
r [] = []
r x = zip ((hd(round(ltll x))),(last(round(ltll x))))

rotate :: Int [(Int,Int)] -> [(Int,Int)]
rotate n x
| n > 1 = rotate (n-1) (r x)
= r x

//Start = rotate 1 [(1,2),(3,4),(5,6)] //[(2,3),(4,5),(6,1)]
//Start = rotate 9 [(1,2),(3,4),(5,6)] //[(4,5),(6,1),(2,3)]
//Start = rotate 234 [(1,2),(3,4),(5,6)] //[(1,2),(3,4),(5,6)]
//Start = rotate 2378475 [(53,73),(35,71),(52,42),(56,78),(42,69),(457,1367),(32,283623),(-363,4643),(0,0),(35,-279427)] //[(4643,0),(0,35),(-279427,53),(73,35),(71,52),(42,56),(78,42),(69,457),(1367,32),(283623,-363)]