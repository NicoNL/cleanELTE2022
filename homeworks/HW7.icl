module HW7
import StdEnv  



/*
1- Task: Write a function that takes a string as input and returns the character that occurs the most
in the string. If there are multiple characters that occur the same maximum number of times, return the
one that appears first in the string.
You can assume that the given string is not empty.
Note: a string is an array of chars!
*/
 
most_common_char :: String -> Char
most_common_char s = last(take whigh list)
where list = [ c \\ c <-: s]
      replist =[ length(filter ((==)z) list) \\ z <-list]
	  highest = last (sort[ length(filter ((==)z) list) \\ z <-list])
	  whigh = length(takeWhile ((<>)highest) replist)+1


//Start = most_common_char "abca" // 'a'
//Start = most_common_char "hello world" //  '1'
//Start = most_common_char "This is a test sentence." //  's'
//Start = most_common_char "123" // '1'



/* 2-
Task: Define a record type for a person that has the following fields:
name: a string that represents the person's name.
age: an integer that represents the person's age.
email: a string that represents the person's email address.
 

Then, write a function that takes a list of person records and returns a
list of the names of people who are over 18 years old and have a Gmail email address
(i.e., their email address ends with "@gmail.com").
 
*/


:: Person = { name :: String
			, age :: Int
			, email :: String
			}
 
p1 :: Person
p1 = {name="Alice", age= 25, email="alice@gmail.com"}
p2:: Person
p2 = {name="Bob", age= 17, email="bob@hotmail.com"}
p3 :: Person
p3 = {name="Charlie", age= 19, email="charlie@gmail.com"}
p4 :: Person
p4 = {name="Dave", age= 22, email="dave@yahoo.com"}

get_qualified_names :: [Person] -> [String]
get_qualified_names p = [ pp.name \\ pp <-p | pp.age > 18 && (toString(dropWhile ((<>)'@') (etl pp.email))) == "@gmail.com"]
where
	  etl :: String -> [Char]
	  etl x = [c \\ c <-: x]

//Start = get_qualified_names [p1, p2, p3, p4] // ["Alice", "Charlie"]
//Start = get_qualified_names [p1, p2, p4] // ["Alice"]
//Start = get_qualified_names [] //  []