module Lect4SelfP
import StdEnv

pi = 1.2

//Write a code which will substitute every True 
//value with 1 and every False value with 0
change :: [Bool] -> [Int]
change [] = []
change [x:xs]
| x == True = [ 1 : change xs]
| x == False = [ 0 : change xs]

//Start = change [True,False,True,False,True]

//Given a list of integers.
//Write a code which will substitute every 
//integer greater than 5 with the character 'g'
// and every integer less or equal to 5 with 's'

intoChar :: [Int] -> [Char]
intoChar [] = []
intoChar [x:xs]
| x > 5 = ['g': intoChar xs]
= ['s': intoChar xs]

//Start = [2,7,1,9,1,3]


//practicing STANDARD FUNCTIONS

//Start = sort[2,5,8,5,6,8,1,1,3]


justeven :: [Int] -> [Int]
justeven [] = []
justeven [x:xs]
| isEven x = [x : justeven xs]
= justeven xs

//Start = justeven [1,2,3,4,5,6,7,8,9,10]

timespi :: [Real] -> [Real]
timespi [] = []
timespi [x:xs] = [(x * pi): timespi xs] 

//Start = timespi [1.2,2.0]

Start = gcd 2 4









