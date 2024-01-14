---
title:                "Haskell recipe: Converting a string to lower case"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case may seem like a simple task, but it can be incredibly useful in various programming scenarios. By converting a string to lower case, you can ensure that your program will handle all input in a consistent and uniform manner, regardless of the original case of the input.

## How To

To convert a string to lower case in Haskell, we can use the `map` function along with the `toLower` function from the `Data.Char` module. The `map` function takes in a function and a list, and applies the function to each element in the list. So, for converting a string to lower case, we can use the `toLower` function as the function and the string as the list.

Let's take a look at an example:

```Haskell
import Data.Char (toLower)

lowercaseString = map toLower "HELLO world"
```

In this code snippet, we first import the `toLower` function from the `Data.Char` module. Then, we use the `map` function to apply the `toLower` function to each character in the string "HELLO world". The resulting string, `lowercaseString`, will be "hello world".

Now, let's try it with a user input:

```Haskell
import Data.Char (toLower)

main = do
    putStrLn "Enter a string:"
    string <- getLine
    let lowercaseString = map toLower string
    putStrLn ("Lowercase string:" ++ lowercaseString)
```

In this example, we first prompt the user to enter a string using the `putStrLn` function. Then, we use the `getLine` function to get the user input and store it in the `string` variable. Next, we use the `let` statement to define a new variable, `lowercaseString`, which will contain the converted string. Finally, we use the `putStrLn` function again to print out the lower case string.

## Deep Dive

In Haskell, strings are represented as lists of characters. This means that we can use list functions, such as `map`, to manipulate strings just like we would manipulate lists. The `toLower` function from the `Data.Char` module takes in a character and returns the lower case version of that character. By using `map` on a string with `toLower`, we are essentially applying `toLower` to each character in the string. This makes converting a string to lower case a simple and elegant solution.

## See Also

- [Haskell Data.Char module documentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
- [Haskell map function documentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:map)