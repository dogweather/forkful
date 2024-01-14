---
title:    "Haskell recipe: Converting a string to lower case"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why
As a popular functional programming language, Haskell offers many useful built-in functions that can make programming tasks easier and more efficient. One such function is `toLower`, which converts a string to lower case. Converting a string to lower case can be necessary when dealing with user input or when performing text manipulation tasks.

## How To
To convert a string to lower case in Haskell, we can use the `toLower` function from the `Data.Char` module. Let's take a look at how we can do this in a simple example:

```Haskell
import Data.Char (toLower)

main = do
    let str = "Hello World"
    putStrLn $ map toLower str
```

In this example, we import the `toLower` function from the `Data.Char` module and then define a string `str` with the value "Hello World". We then use the `putStrLn` function to print out the result of `map toLower str`, which applies the `toLower` function to each character in the string and returns a new string. The output of this program would be:

```
hello world
```

We can also use the `toLower` function with user input, as shown in the example below:

```Haskell
import Data.Char (toLower)

main = do
    putStrLn "Please enter a string: "
    str <- getLine
    putStrLn $ map toLower str
```

In this example, we use the `getLine` function to get user input and then apply the `toLower` function to the input string and print out the result.

## Deep Dive
The `toLower` function in Haskell relies on the Unicode standard, which defines the mapping between upper and lower case characters for various languages. This means that the `toLower` function can handle characters from different languages and properly convert them to lower case.

One important thing to note is that the `toLower` function only converts alphabetic characters to lower case. This means that numbers, symbols, and other special characters will remain unchanged.

Additionally, the `toLower` function does not modify the original string, but instead returns a new string with the converted characters. This is because Haskell is a purely functional language, meaning that functions can't have side effects and can only return new values.

## See Also
- [Haskell Documentation on `Data.Char`](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.14.1.0/Data-Char.html#v:toLower)
- [Haskell Tutorial: Functions and Types](https://www.tutorialspoint.com/haskell/haskell_functions_and_types.htm)
- [Unicode Database](https://unicode.org/Public/UNIDATA/UnicodeData.txt)