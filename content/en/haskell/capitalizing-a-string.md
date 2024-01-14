---
title:    "Haskell recipe: Capitalizing a string"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why

Capitalizing a string may seem like a simple task, but it can be incredibly useful in programming. By capitalizing words in a string, we can make our code more readable and easier to understand.

## How To

To begin, we first need to import the `Data.Char` library in order to use the `toUpper` function. This function takes a character as input and returns the uppercase version of that character. For example:

```Haskell
import Data.Char

toUpper 'h' -- returns 'H'
```

Now that we have the `toUpper` function, we can use it to capitalize a string. We can do this by first splitting the string into a list of individual characters using the `words` function. Then, we map the `toUpper` function onto each character in the list. Finally, we use the `unwords` function to join the capitalized characters back into a string.

```Haskell
capitalize :: String -> String
capitalize str = unwords (map toUpper (words str))

capitalize "hello world" -- returns "HELLO WORLD"
```

We can also choose to only capitalize the first letter of the string by using the `head` and `tail` functions. We can then combine the first capitalized letter with the rest of the string using the `++` operator.

```Haskell
capitalizeFirst :: String -> String
capitalizeFirst str = toUpper (head str) ++ tail str

capitalizeFirst "hello world" -- returns "Hello world"
```

## Deep Dive

Now that we have seen how to capitalize a string, let's take a deeper look at the `words` and `unwords` functions that we used. The `words` function splits a string into a list of words, while the `unwords` function joins a list of words back into a string. These functions are useful for manipulating strings in various ways, not just for capitalization.

Additionally, we can also use the `toUpper` function with other data types, such as characters, integers, and even lists. This shows the versatility and power of Haskell's built-in functions.

## See Also

- [Haskell String Functions](https://www.tutorialspoint.com/haskell/haskell_string_functions.htm)
- [Data.Char Library documentation](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)