module HW2
import StdEnv



//Please write your Neptun code here: A0T4ZR
/*
Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add
your tests as well. Don't change the given function signatures, however, you can add as many functions as
you wish, just make sure to name them appropriately (if a function squares the number, call it 'square',
'second_power', etc. and not 'f' or 'g'). The same goes for variable names.

Make sure that you comment all 'Start'-s before submitting the code.


Hints:
    1. The basis of functional programming is functions, if a problem can't be solved
    with one function divide it into multiple functions.
    2. To abort with a message you can use the 'abort' function.
    3. The logic needs to be thought by you, the implementation you can figure out
    by going through lecture slides and practice material.
*/


// SOLVE TWO OF THE FOLLOWING TASKS:




/* 1- Top K Frequent Elements
    Given an integer list(nums) and an integer(k), return the k most frequent elements.
    You may return the answer in any order.
    You may assume that the input is valid, you don't have to validate the input,
    meaning that for example, you won't get [1,1,1] 3 or [1,1,1,3,4,5] 2.
*/

//Start = repet [1,1,1,2,2,1,1,2,2,1,1,1,3,4,5,5,5,2,2,2]
 
//top_k_freq_elems :: [Int] Int -> Int






// Start = top_k_freq_elems  [1,1,1,2,2,3] 2 // [1,2]
// Start = top_k_freq_elems  [1,1,2,1,2,3,3,3,3] 3 // [3,2,1]
// Start = top_k_freq_elems  [1] 1 // [1]
// Start = top_k_freq_elems  [3,3,3] 1 // [3]
// Start = top_k_freq_elems  [1,2,3] 3 // [1,2,3]
// Start = top_k_freq_elems  [1,3,2,3] 3 // [1,2,3]



/*2- You are given a list of characters(let us call it letters) that is sorted in non-decreasing order,
    and a character (target). There are at least two different characters in "letters".

    Return the smallest character in "letters" that is lexicographically greater than target.
    If such a character does not exist, return the first character in "letters".

    Example 1:
        Input: letters = ['c','f','j'], target = 'a'
        Output: 'c'
        Explanation: The smallest character that is lexicogrpahically greater than 'a' in letters is 'c'.

    Example 2:
        Input: letters = ['c','f','j'], target = 'c'
        Output: 'f'
        Explanation: The smallest character that is lexicographically greater than 'c' in letters is 'f'.

    Example 3:
        Input: letters = ['x','x','y','y'], target = 'z'
        Output: 'x'
        Explanation: There are no characters in letters that is lexicographically greater than 'z' so we return letters[0].
   

    Constraints:
    2 <= letters.length <= 104
    letters at position "i" is a lowercase English letter.
    letters are sorted in non-decreasing order.
    letters contain at least two different characters.
    target is a lowercase English letter.
*/

findGreater :: [Char] Char -> Char
findGreater charlist c 
| c > (last (charlist)) = hd charlist
findGreater [x:xs] c
| x > c = x
| x == c = findGreater xs c
| x < c = findGreater xs c


//Start = findGreater ['c','f','j'] 'c'

//Start = findGreater ['c','f','j'] 'c'
//Start = findGreater ['c','f','j'] 'c' // 'f'
//Start = findGreater ['x','x','y','y'] 'z' // 'x'
// Start = findGreater ['a','b','c'] 'd' // 'a'
// Start = findGreater ['c','f','j'] 'a' // 'c'  


/*3- Given a list of integers, write a function that iterates through every element and returns a list of lists of Fibonacci sequences as shown in the example.
    The elements of the list indicate how many Fibonacci numbers are in the corresponding sublist and the Fibonacci sequence will continue in the next sublist.
    (Assume there is no negative integer in the list)
   
    example: fibList [3,2,1,2] = [[1,1,2],[3,5],[8],[13,21]]
             fibList [2,5,3] = [[1,1],[2,3,5,8,13],[21,34,55]]
*/

//fib1 finds the n number from the fibonacci sequence
fib1 :: Int -> Int
fib1 n
| n == 0 = 2
| n == 1 = 1
| n > 1 = fib1(n-1) + fib1(n-2)
//Start = fib1 4
//fib 2 creates a list of the first n fibonacci numbers
//| n > 0 = reverse([fib1 n] ++ reverse(fib2 (n-1)))
fib2 :: Int -> [Int]
fib2 n
| n > 0 = [fib1(n-1)] ++ fib2 (n-1)

Start = fib2 5
//fib 3 creates a fibonacci numbers list from n fibonacci number| a= position in fibonacci sequence b++
fib3 :: Int Int -> [Int]
fib3 a b
| b == 0 = []
| b > 0 = [fib1 a] ++ (fib3 (a+1) (b-1))


fibList2 ::Int [Int] -> [[Int]]
fibList2 n [] = []
fibList2 n [x:xs] = [fib3 (n+1) x:fibList2 nl xs]
where l = length(fib3 x (n+1))
	  nl = x + n 

//Start =  fibList2 4 [5,3,2]

fibList::[Int] -> [[Int]]
fibList [] = []
fibList [x:xs] = [fib2  x : fibList2 l xs ]

where l = length(fib2 x)
	  
	  
//Start = fibList [3,2,1,2]
 

//Start = fibList [4,5,3,2] // [[1,1,2,3],[5,8,13,21,34],[55,89,144],[233,377]]
//Start = fibList [] // []
//Start = fibList [10,5] // [[1,1,2,3,5,8,13,21,34,55],[89,144,233,377,610]]