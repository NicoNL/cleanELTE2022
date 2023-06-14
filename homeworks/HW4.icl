module HW4
import StdEnv

/* Task 1
    Given a list of characters, extract all the vowel
    characters and count them. After that, cipher the list of characters
    by that count. Ciphering here means just shift the character by that count
   
        For Example:
            let's assume that the vowels count is 2, then:
                'a' + 2 = 'c' ... Here we ciphered 'a' into 'c'
                'c' + 2 = 'e' ... We did the same as above
                Therefore, by ciphering, we just shift the letter into its
                N's neighbor.
           
    Task Example:
        Input is ['m', 'o', 'h', 'i','d','o']  
        count of vowels is 3 (o,i,o)
        We cipher the list:
        ['m', 'o', 'h', 'i','d','o']   ->   ['p','r','k','l','g','r']
       
    Reminder: English vowels are: a, e, i, o, and u.
   
    Note: To shift a letter by N, you can do the following:
        toChar (fromChar letter + n)
*/
justV :: [Char] -> [Char]
justV [] = []
justV [x:xs]
| x == 'a' = [ x : justV xs]
| x == 'e' = [ x : justV xs]
| x == 'i' = [ x : justV xs]
| x == 'o' = [ x : justV xs]
| x == 'u' = [ x : justV xs]
= justV xs

cipherList :: [Char] -> [Char]
cipherList x = map (\ x = toChar (fromChar x + n)) x
where n = length(justV x)

//Start = cipherList ['m','o','h','i','d','o']            //['p','r','k','l','g','r']
//Start = cipherList ['t', 'a', 'r', 'i', 'q']          //  ['v','c','t','k','s']
//Start = cipherList ['b', 'e', 'k', 'a']               //  ['d','g','m','c']
//Start = cipherList ['a','b','d','u','l','l','a','h']  //  ['d','e','g','x','o','o','d','k']



/* Task 2
    Given two integers, insert the second integer to the first one.
    The insertion should be as follows:
        After each digit considered in the first integer,
        you insert a digit from the second integer.
   
    Example:
        123 321 -> 132231
        13 13   -> 1133
       
    So after each digit in first integer, you insert a digit from
    the second integer.
   
    Note that both given numbers are of equal length.
*/
ntl :: Int -> [Int]
ntl x
| x > 10 = [x rem 10] ++ ntl (x/10)
| x == 10 = [0,1]
= [x]

mix :: [Int] [Int] -> [Int]
mix [] [] = []
mix [x] [y] = [x,y]
mix [x,y:xs] [a,b:as] = [x,a] ++ mix [y:xs] [b:as]

its :: [Int] -> String
its[] = ""
its [x:xs] = (toString x) +++ its xs

intInsertion :: Int Int -> Int
intInsertion a b = toInt(its(abl))
where al = reverse (ntl a)
	  bl = reverse (ntl b)
	  abl = mix al bl


//Start = intInsertion 123 123 // 112233
//Start = intInsertion 123 321 // 132231
//Start = intInsertion 13 13 // 1133
//Start = intInsertion 1 2 // 12
//Start = intInsertion 2 1 // 21