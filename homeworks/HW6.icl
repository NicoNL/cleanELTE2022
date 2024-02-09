module HW6
import StdEnv

/*1. Given the list of integers, modify it in the following way:
I. Remove all numbers which are multiple of 3
II. Sort the remaining list in descending order
III. Swap 1st and 2nd elements, 3rd and 4th, 5th and 6th, etc.
*/

shuffleSort :: [Int] -> [Int]
shuffleSort x = swap([ x \\ x <- y | not( x rem 3 == 0)])
where y = reverse(sort x)
      swap [] = []
      swap [x] = [x]
      swap [x,y] = [y,x]
      swap [x,y:xs] = [y,x] ++ swap xs

//Start = shuffleSort [4,3,2] // [2,4]
//Start = shuffleSort [4,1,3,2,5,6,7] // [5,7,2,4,1]
//Start = shuffleSort [3,6,3,9,12] // []
//Start = shuffleSort [2,4,5,7,14,17] // [14,17,5,7,2,4]
//Start = shuffleSort [] // []


/*2. Given a list of lists of real numbers, for every sublist   find the item in the  sublist which is closest
to the average of the sublist.
e.g [[1.3, 5.2, 7.7, -2.3, 23.45] , [3.0,8.4] ] ->  avg of [1.3, 5.2, 7.7, -2.3, 23.45]  is 7.07 so the closest value from the list is  7.7
similarly, avg of [3.0,8.4] is 5.7  so the closest value from the list is  3.0
*/
closest :: [Real] Real -> Real
closest [x] z = x
closest [x,y] z
| abs(x-z) <  abs(y-z) = x
| abs(x-z) == abs(y-z) = x
= y
closest [x,y:xs] z
| abs(x-z) <  abs(y-z) = closest [x:xs] z
| abs(x-z) ==  abs(y-z) = closest [x:xs] z
= closest [y:xs] z
 
closestToAvg :: [[Real]] -> [Real]
closestToAvg [] = []
closestToAvg [x:xs] = [closest x (avr x)] ++ closestToAvg xs
where avr x = toReal(sum x)/toReal(length x)

//Start = closestToAvg [[1.3, 5.2, 7.7, -2.3, 23.45] , [3.0,8.4] ] //  [7.7,3]
//Start = closestToAvg [[2.4 ,4.5 ,6.7 ,6.6 ,7.7] , [5.6 , 6.8 ,4.8 , 4.1] , [5.5,5.1] , [5.0] , [7.8] ] // [6.6,5.6,5.5,5,7.8]
//Start = closestToAvg [[1.3]] // [1.3]