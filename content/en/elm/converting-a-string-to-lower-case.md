---
title:                "Elm recipe: Converting a string to lower case"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

String manipulation is a fundamental aspect of programming, and one commonly used operation is converting a string to lower case. This can be useful in various scenarios, such as comparing two strings without worrying about capitalization or generating user-friendly output.

## How To

Before diving into the code, it's important to note that strings in Elm are immutable, meaning they cannot be modified directly. Instead, a new string with the desired changes needs to be created. With that in mind, let's see how we can convert a string to lower case in Elm:

```Elm
import String exposing (toLower)

toLower "Elm Programming" -- Outputs "elm programming"
```

As seen in the code, the `toLower` function from the `String` module takes in a string as input and returns the lower case version of that string. It handles all special characters and accented letters as well.

```Elm
toLower "STRIng convertER 123" -- Outputs "string converter 123"
```

The `toLower` function also works with non-alphabetical characters, such as numbers, symbols, and spaces.

## Deep Dive

For those interested in understanding the inner workings of the `toLower` function, here's a deeper look at how it handles string conversion.

The `toLower` function works by iterating through each character in the original string and checking if it is a capital letter. If so, it uses the `Char.toCode` function to get the Unicode code point for that character, adds 32 to it (which is the difference between the ASCII code for upper and lower case letters), and then converts it back to a character using the `Char.fromCode` function.

This process is repeated for each character in the string, and the resulting characters are combined to create the lower case version of the original string.

## See Also

- Official Elm Documentation for String Module: https://package.elm-lang.org/packages/elm/core/latest/String
- Interactive String manipulation tool: https://elm-lang.org/0.19.1/tools/make-elm-value
- Article on String techniques in Elm: https://dev.to/jfmengels/extended-real-world-example-of-using-elm-for-web-trading-pace-calculator-3hee