---
title:    "Haskell recipe: Capitalizing a string"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
Capitalizing strings may seem like a simple task, but it is a common problem that programmers encounter. Whether you are working on a text editing program or parsing user input, string capitalization is often required. Thankfully, Haskell offers an elegant solution to this problem that is both efficient and easy to implement.

## How To
To capitalize a string in Haskell, we can use the `toUpper` function from the `Data.Char` module. This function takes a single character as an argument and returns its uppercase equivalent. We can use this function in combination with the `map` function to apply it to every character in our string.

Here is an example of a simple function that takes in a string and capitalizes it using the `toUpper` function:
```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize str = map toUpper str
```

Let's try using our `capitalize` function on a string and see what the output looks like:
```Haskell
main = do
  let str = "hello world"
  let result = capitalize str
  print result
```
Output: `HELLO WORLD`

As we can see, our function successfully capitalized every letter in the string. However, if we want to capitalize only the first letter of the string, we can modify our function to use the `toUpper` function on only the first character. Here is an example:
```Haskell
import Data.Char (toUpper)

capitalizeFirst :: String -> String
capitalizeFirst str = toUpper (head str) : tail str
```

Let's test our new `capitalizeFirst` function and see the output:
```Haskell
main = do
  let str = "hello world"
  let result = capitalizeFirst str
  print result
```
Output: `Hello world`

We can see that our function successfully capitalized the first letter while leaving the rest intact.

## Deep Dive
In Haskell, strings are represented as lists of characters. This is why we were able to use the `map` function to apply `toUpper` to every character in our string. The `toUpper` function works by converting the character to its ASCII value and then subtracting 32 from it to obtain the uppercase equivalent. This ASCII manipulation is what makes the `toUpper` function more efficient than simply checking for lowercase and returning the uppercase equivalent.

Another important point to note is that the `toUpper` function only works for characters that have an uppercase equivalent. This means if we try to use it on a special character or a number, it will return an error. Therefore, it is important to handle edge cases when implementing string capitalization in your code.

## See Also
- Learn You a Haskell for Great Good: [String functions](http://learnyouahaskell.com/starting-out#strings)
- Real World Haskell: [Strings and Characters](http://book.realworldhaskell.org/read/strings-and-characters.html)
- Haskell Documentation: [Data.Char module](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)