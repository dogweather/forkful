---
title:                "Capitalizing a string"
html_title:           "Haskell recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
Capitalizing strings may seem like a trivial task, but it can have a big impact on the readability and aesthetics of your code. By learning how to properly capitalize strings in Haskell, you can enhance the quality of your code and make it more visually appealing.

## How To
To capitalize a string in Haskell, we can use the `toUpper` function from the `Data.Char` module. Here's an example code with a sample output:

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize str = map toUpper str

main = do
  let str = "hello world"
  putStrLn $ capitalize str
```
Output: `HELLO WORLD`

In the code above, we first import the `toUpper` function from the `Data.Char` module. Then, we define a function `capitalize` that takes a string as input and uses the `map` function to apply the `toUpper` function to each character in the string. In the `main` function, we create a string and pass it to the `capitalize` function, then print the output using `putStrLn`.

## Deep Dive
In Haskell, strings are treated as lists of characters, which means we can use all the list operations on strings. This is why we were able to use the `map` function in the `capitalize` function to apply the `toUpper` function to each character in the string. Additionally, the `toUpper` function only works on single characters, so using it directly on a string would result in an error. This is why we had to use the `map` function to apply it to each character.

## See Also
- [Haskell Documentation on `Data.Char`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
- [Tutorial on Strings in Haskell](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [List Functions in Haskell](https://www.haskell.org/tutorial/arrays.html#sect21.2)