---
title:                "Haskell recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to capitalize a string in Haskell? Maybe you are working on a program that requires proper title case formatting or you just want to present data in a more visually appealing way. Regardless of the reason, capitalizing strings is a common task in programming and can easily be done in Haskell. 

## How To
Coding in Haskell can seem intimidating at first, but once you understand the basics, performing tasks like capitalizing a string becomes much easier. Let's take a look at a simple example using the `map` function and the `toUpper` function from the `Data.Char` library. 

````Haskell
import Data.Char (toUpper)

capitalizeString :: String -> String
capitalizeString str = map toUpper str

main :: IO ()
main = do
  let myString = "hello world"
  let capitalizedString = capitalizeString myString
  putStrLn capitalizedString
```

The `capitalizeString` function takes in a string as an argument and uses `map` to apply the `toUpper` function to each character in the string, returning a new string with all uppercase letters. In the `main` function, we create a string and then apply our `capitalizeString` function to it, printing out the result. 

The output of this code would be: 
```
HELLO WORLD
```

Pretty simple, right? But what if we wanted to not only capitalize the first letter of each word, but keep the rest of the letters lowercase? This is where a deep dive into capitalizing strings in Haskell comes in. 

## Deep Dive 
To achieve our desired result, we can use the `words` function from the `Data.List` library to split the string into a list of words, then apply `capitalizeString` to the first word, and finally use `unwords` to rejoin the words back together into a string. 

````Haskell 
import Data.Char (toUpper)
import Data.List (words, unwords)

capitalizeString :: String -> String
capitalizeString str =
  let
    helper [] = []
    helper (x:xs) = toUpper x : xs
  in
    unwords $ map helper (words str)

main :: IO ()
main = do
  let myString = "hello world"
  let capitalizedString = capitalizeString myString
  putStrLn capitalizedString
```

The `helper` function takes in a list of characters and recursively applies `toUpper` to the first character, then adds on the remaining characters unchanged. We then `map` this function to each word in our string, and use `unwords` to rejoin the words back together, giving us our desired capitalized string. 

The output of this code would be: 
```
Hello world
```

## See Also
- [Haskell Documentation](https://www.haskell.org/documentation/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Real World Haskell](http://book.realworldhaskell.org/)