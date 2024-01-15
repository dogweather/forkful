---
title:                "文字列の大文字化"
html_title:           "Haskell: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
Konnichiwa! Do you ever find yourself needing to capitalize a string in your programming? Maybe you want to make the first letter of a name or title uppercase. Or perhaps you just prefer the look of a capitalized string. Whatever the reason may be, capitalizing a string is a common task in programming and this article will show you how to do it in Haskell.

## How To
To capitalize a string in Haskell, we can use the `toUpper` function from the `Data.Char` module. Let's take a look at some simple examples:

```
import Data.Char

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

capitalize "hello" -- "Hello"

capitalize "haskell" -- "Haskell"
```

In the code above, we first import the `Data.Char` module which contains the `toUpper` function. Then, we define a `capitalize` function that takes a `String` as input and outputs a capitalized version of that string. We use pattern matching to check if the string is empty and if it is, we simply return an empty string. Then, in the second line, we use the `toUpper` function to capitalize the first letter of the string and concatenate it with the rest of the string. 

We can also use `map` and `toUpper` to capitalize each letter in a string:

```
import Data.Char

capitalize :: String -> String
capitalize "" = ""
capitalize str = map toUpper str

capitalize "hello" -- "HELLO"

capitalize "haskell" -- "HASKELL"
```

In the code above, we use `map` to apply the `toUpper` function to each character in the string. This will give us the same result as the previous function, but with a slightly different approach. 

## Deep Dive
Now, let's take a deeper look at how the `toUpper` function works. In Haskell, characters are represented by numbers using the ASCII system. The lowercase letters range from 97 to 122, while the uppercase letters range from 65 to 90. The `toUpper` function simply takes a character and adds 32 to it (subtracting 32 would give us the lowercase version of the character). This may seem like a strange method, but it is rooted in the history of computing and character representation.

If you want to delve even deeper, you can take a look at the source code for `toUpper` in the `Data.Char` module. It uses a `case` statement to check where the character falls in the ASCII range and applies the appropriate transformation. This is just one example of the power and flexibility of Haskell's pattern matching and functional programming.

## See Also
If you want to learn more about string manipulation in Haskell, here are some helpful resources to check out:

- [Haskell documentation for Data.Char module](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
- [Learn You a Haskell for Great Good! - Chapter 7: Modules](http://learnyouahaskell.com/modules)
- [Real World Haskell - Chapter 2: Types and Functions](http://book.realworldhaskell.org/read/types-and-functions.html)