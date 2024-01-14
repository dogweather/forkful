---
title:    "Haskell recipe: Converting a string to lower case"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why: The Importance of Converting Strings to Lower Case

In programming, text is commonly represented as strings. And when working with strings, it is often necessary to convert them to lower case. This is important for several reasons, including data consistency, user input handling, and string comparison. In this blog post, we will explore why converting strings to lower case is important and how to do it in Haskell.

## How To: Converting Strings to Lower Case in Haskell

In Haskell, converting a string to lower case can be done using the `map` function and the `toLower` function from the `Data.Char` library. Let's see an example below:

```Haskell
import Data.Char

-- convert a string to lower case
toLowerString :: String -> String
toLowerString str = map toLower str

main = do
  let originalString = "HELLO WORLD"
  let lowercaseString = toLowerString originalString
  -- print original string and converted string
  putStrLn $ "Original string: " ++ originalString
  putStrLn $ "Converted string: " ++ lowercaseString
```
Output:
```
Original string: HELLO WORLD
Converted string: hello world
```

In the code above, we first import the necessary library `Data.Char` which contains the `toLower` function. Then, we define a function `toLowerString` which takes a string as an input and converts it to lowercase using `map` and `toLower` functions. In the `main` function, we create a string `originalString` and pass it to our `toLowerString` function, storing the result in a new string `lowercaseString`. Finally, we print both the original string and the converted string.

## Deep Dive: Understanding String Conversion in Haskell

In Haskell, strings are immutable, meaning they cannot be changed in-place. So when we convert a string to lower case, a new string is created with the converted characters. This helps maintain data consistency and avoids any unwanted side effects. Also, the `map` function in Haskell applies a given function to every element in a list, in this case, the `toLower` function. This makes converting strings to lower case very efficient and concise.

It is also worth noting that the `toLower` function in Haskell works based on the Unicode character set. This means that it can handle non-English characters and symbols as well.

## See Also

- [Official Haskell Documentation on Strings](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-String.html)
- [Haskell Wiki on String Functions](https://wiki.haskell.org/Strings)
- [Tutorial on String Manipulation in Haskell](https://www.tutorialspoint.com/haskell-programming/haskell_strings.htm)