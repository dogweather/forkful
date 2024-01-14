---
title:                "Haskell recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Why

In the world of programming, manipulating strings is a common task. One such manipulation is converting a string to lowercase. This may be necessary when comparing strings or ensuring consistency in formatting. In this post, we will explore how to convert a string to lowercase in Haskell.

# How To

To convert a string to lowercase in Haskell, we can use the `toLower` function from the `Data.Char` module. Here is an example code block:

```Haskell
import Data.Char

strToLower :: String -> String
strToLower str = map toLower str

main = do
  let myString = "HELLO WORLD"
  let lowerString = strToLower myString
  putStrLn lowerString
```

The output of this code will be `hello world`, with all letters converted to lowercase. Let's break down the code:

- First, we import the `Data.Char` module, which contains the `toLower` function.
- Next, we define a function `strToLower` that takes in a string and uses `map` to apply the `toLower` function to each character in the string.
- Finally, in our `main` function, we define a string `myString` and convert it to lowercase using our `strToLower` function. We then print the lowercase string using `putStrLn`.

# Deep Dive

The `toLower` function works by converting each character to its lowercase equivalent. It follows the Unicode standards, meaning it can handle characters in different languages. For example, the string `"HÉLLÖ WÖRLD"` would be converted to `"héllö wörl"`. It also takes into account special characters and symbols, converting them to their lowercase version as well.

It's important to note that the `toLower` function only works on characters, not entire strings. That is why we used the `map` function in our example to apply it to each character in the string. Additionally, this function does not modify the original string, but rather returns a new lowercase string.

# See Also

For more information on the `toLower` function and other string manipulation functions in Haskell, check out these links:

- [Haskell documentation for Data.Char module](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
- [Haskell String Manipulation Tutorial from Tutorialspoint](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [Haskell String Functions Cheat Sheet](https://www.springfieldspringfield.co.uk/file_download.php?url=./guide._
to._haskell/extras/string_functions.txt)