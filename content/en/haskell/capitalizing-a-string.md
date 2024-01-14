---
title:                "Haskell recipe: Capitalizing a string"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
Capitalizing a string is a common task in many programming languages, including Haskell. It involves changing the case of letters in a string from lowercase to uppercase. This can be useful for formatting purposes or for comparing strings.

## How To
To capitalize a string in Haskell, we can use the `toUpper` function from the `Data.Char` module. This function takes a character as input and returns the uppercase version of that character. We can then use the `map` function to apply `toUpper` to each character in our string. Here is an example code showing how to capitalize a string and its output:

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String 
capitalize str = map toUpper str

main :: IO ()
main = do
  let input = "hello world"
  putStrLn (capitalize input)
```

Output:
```
HELLO WORLD
```

In this code, we first import the `toUpper` function from the `Data.Char` module. Then, we define our own `capitalize` function that takes a string as input and uses `map` to apply `toUpper` to each character in the string. Finally, in our `main` function, we create a variable `input` with the string "hello world" and pass it to our `capitalize` function before printing the output.

## Deep Dive
While the `toUpper` function from the `Data.Char` module is a convenient way to capitalize a string, it may not always produce the desired result. For instance, in some languages, there are special characters that have lowercase and uppercase versions, such as the German "ß" and "SS". The `toUpper` function in Haskell does not take this into account and will simply convert "ß" to "SS" instead of "ẞ".

To handle these special cases, we can use the `toTitle` function from the `Data.Char` module instead. This function has the same functionality as `toUpper`, but it also takes into account special cases for characters with diacritics, such as "ẞ". This ensures that our string is properly capitalized without losing any important characters.

## See Also
- [Haskell Documentation - Data.Char module](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
- [Learn You a Haskell - String Manipulation](http://learnyouahaskell.com/starting-out#learn-you-a-haskell-for-great-good)
- [Real World Haskell - Strings and Characters](http://book.realworldhaskell.org/read/strings-and-characters.html)