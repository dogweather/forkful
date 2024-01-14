---
title:                "Elm recipe: Using regular expressions"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are powerful tools for manipulating and searching text. They can help you validate user input, extract data from strings, and even transform data in your Elm programs. Learning how to use regular expressions can greatly improve your productivity and make your code more dynamic.

## How To

To use regular expressions in Elm, you will need to import the `Regex` module. The `Regex` module provides functions for creating, manipulating, and matching regular expressions.

To create a regular expression, we use the `Regex.fromString` function. For example, let's create a regular expression to match a phone number in the format of `###-###-####`:

```Elm
import Regex exposing (Regex)

phoneNumberRegex : Regex
phoneNumberRegex =
    Regex.fromString "\\d{3}-\\d{3}-\\d{4}"
```

Once we have our regular expression, we can use the `Regex.find` function to search for a match within a string:

```Elm
import Regex exposing (Regex)

phoneNumberRegex : Regex
phoneNumberRegex =
    Regex.fromString "\\d{3}-\\d{3}-\\d{4}"

phoneNumberString : String
phoneNumberString =
    "555-555-5555"

Regex.find phoneNumberRegex phoneNumberString
--> Just (Matched "555-555-5555")
```

As you can see, the `Regex.find` function returns a `Maybe Regex.Match` type, indicating whether a match was found or not. If a match is found, the `Matched` constructor returns the actual matched string.

We can also use regular expressions for replacements using the `Regex.replace` function. For example, let's replace all occurrences of the word "Elm" with "Regular Expressions" in a string:

```Elm
import Regex exposing (Regex)

replaceRegex : Regex
replaceRegex =
    Regex.fromString "Elm"

inputString : String
inputString =
    "I love Elm programming!"

Regex.replace replaceRegex inputString "Regular Expressions"
--> "I love Regular Expressions programming!"
```

## Deep Dive

Regular expressions are made up of symbols and characters that follow a specific pattern. Some commonly used symbols are:

- `.` - matches any single character
- `+` - matches one or more occurrences of the preceding character or group
- `*` - matches zero or more occurrences of the preceding character or group
- `?` - matches zero or one occurrence of the preceding character or group
- `^` - matches the beginning of a string
- `$` - matches the end of a string
- `[]` - matches any character within the brackets
- `()` - groups the enclosed characters together
- `|` - matches either the expression before or after the symbol

To learn more about the different symbols and their usage, I recommend checking out the official Elm documentation on regular expressions or trying out different regular expressions on websites like RegExr or Regex101.

## See Also

For more information on regular expressions in Elm, check out the official Elm documentation: https://package.elm-lang.org/packages/elm/regex/latest/

To practice and test your regular expressions, check out these websites: https://regexr.com/ and https://regex101.com/