module HW3
import StdEnv

//NEPTUN CODE: A0T4ZR
/* Narcissistic Numbers
A Narcissistic Number is a number that is the sum of its own digits each raised to the power of the number of digits.
For example, 153 is a Narcissistic Number since 1^3 + 5^3 + 3^3 = 153.

Write a function that filters the Narcissistic numbers from a given list of positive integers.
*/
itl :: Int -> [Int]
itl n
| n < 10 = [n]
= [(n rem 10)] ++ (itl (n / 10))

NChecker :: Int -> Bool
NChecker n = (foldr (+) 0 (map(\ x = x^l) ntl)) == n
where ntl = itl n
	  l = length(ntl)

filter_Narcissistic_nums :: [Int] -> [Int]
filter_Narcissistic_nums lista = filter NChecker lista

//Start = filter_Narcissistic_nums [370,371,407,22,1634,77,8208,9474] // [370,371,407,1634,8208,9474]
//Start = filter_Narcissistic_nums [54748,92727,93084] // [54748,92727,93084]
//Start = filter_Narcissistic_nums [1..9] // [1,2,3,4,5,6,7,8,9]



/*
Write a function that takes in a list of real numbers and returns the maximum absolute difference between
any two adjacent numbers in the list. For example, if the input list is [1.0, 2.0, 1.5, 4.0, 3.5], the function
should return 2.5, which is the absolute difference between 1.5 and 4.0.
*/
Dlist :: [Real] -> [Real]
Dlist [x,y] = [(abs(x-y))]
Dlist [x,y:xs] = [(abs(x-y))] ++ Dlist [y:xs]

task2 :: [Real] -> Real
task2 lista = maxList (Dlist lista)

//Start = task2 [1.0, 1.0, 1.0, 1.0] // 0.0
//Start = task2  [1.0, 2.0, 1.5, 4.0, 3.5] // 2.5
//Start = task2 [1.0, 5.0, 2.0, 7.0] // 5.0
//Start = task2 [3.14, 1.618, 2.718, 0.577, 1.414] // 2.141