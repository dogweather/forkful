---
title:    "Elm recipe: Capitalizing a string"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing strings is a common practice in programming and can serve a variety of purposes. It can be used for better readability, proper formatting, or to meet certain requirements for data processing. In Elm, capitalizing strings can be easily achieved with a few lines of code.

## How To

To capitalize a string in Elm, we can use the `String.toUpper` function. This function takes in a string as an argument and returns the same string with all characters converted to uppercase.

```Elm
stringToUpper : String -> String
stringToUpper "hello world" -- outputs "HELLO WORLD"
```

Alternatively, if we only want the first letter of the string to be capitalized, we can use the `String.toTitle` function. This function capitalizes the first letter of each word in the string.

```Elm
stringToTitle : String -> String
stringToTitle "hello world" -- outputs "Hello World"
```

We can also use `String.fromList` and `String.fromChar` to build our own functions for capitalization. `String.fromList` converts a list of characters into a string, while `String.fromChar` converts a single character into a string. Using these functions, we can loop through each character in the string and convert it to uppercase or title case accordingly.

## Deep Dive

When it comes to capitalizing strings, there are a few factors to consider. One of the main considerations is the language being used. Some languages have specific rules for capitalization, such as in German where all nouns are capitalized. Therefore, it is important to understand the specific requirements for the language you are working with.

Another aspect to consider is performance. While using built-in functions like `String.toUpper` and `String.toTitle` is convenient, it may not be the most efficient way to capitalize strings. It is always worth exploring alternative methods, such as using regular expressions, for better performance.

Finally, it is important to handle special characters and diacritics when capitalizing strings. Languages like French or Spanish may have characters with accents that need to be properly converted to uppercase.

## See Also

For more information on string manipulation in Elm, check out the official documentation [here](https://package.elm-lang.org/packages/elm/core/latest/String). You can also explore different approaches to capitalization in Elm on the Elm community forum [here](https://discourse.elm-lang.org/t/capitalizing-a-string/5236).