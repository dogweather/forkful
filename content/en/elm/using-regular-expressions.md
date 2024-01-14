---
title:    "Elm recipe: Using regular expressions"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Why

Regular expressions (regex) are a powerful tool for text manipulation and data validation. They allow us to search, find and replace, and extract information from strings with a high level of precision and flexibility. In Elm, regular expressions can be used to handle various user input and create robust and efficient code.

## How To

First, we need to import the `Regex` module from the standard library. This will allow us to use all the functions and operators related to regular expressions. Then, we can use the `Regex.fromStr` function to create a regex value from a string.

```Elm
import Regex

myRegex = Regex.fromStr "[A-Z]+"
```

The above code creates a regex that will match any string containing one or more uppercase letters. Now, let's see how we can use it to validate user input. Suppose we want to validate a user's name, which should start with an uppercase letter and can contain only letters and spaces. We can use the `match` function to check if the user input matches our regex.

```Elm
isValidName : String -> Bool
isValidName name =
    Regex.match myRegex name
```

Note that the `match` function returns a `Maybe` value, so we need to handle the cases where the input is invalid. We can use pattern matching or the `Maybe.withDefault` function to handle these cases.

```Elm
isValidName : String -> Bool
isValidName name =
    case Regex.match myRegex name of
        Just _ ->
            True

        Nothing ->
            False

-- Or using `withDefault`

isValidName : String -> Bool
isValidName name =
    Regex.match myRegex name |> Maybe.withDefault False
```

We can also use regular expressions for searching and replacing parts of a string. Let's say we have a string containing various words, and we want to replace all the instances of the word "cat" with "dog". We can use the `replace` function for this.

```Elm
myString = "I have a cat named Mittens and a cat named Whiskers."

Regex.replace (Regex.fromStr "cat") (always "dog") myString
-- Output: "I have a dog named Mittens and a dog named Whiskers."
```

## Deep Dive

Regular expressions can be a bit daunting at first, with all the symbols and operators that can be used to create different patterns. In Elm, we have access to a variety of functions and operators that make it easier to create and manipulate regular expressions. Some useful functions include `find`, `replace`, `match`, and `contains`. We also have useful operators such as `|>`, `>>`, and `<<` that make it easy to chain and compose regex functions.

It's essential to understand the basic syntax and rules of regular expressions. Some common symbols and operators include:

- `.` matches any character except newline
- `*` matches any number of the previous character
- `+` matches one or more of the previous character
- `?` matches zero or one of the previous character
- `\` escapes special characters
- `[]` matches any character inside the brackets
- `|` matches either the preceding or the next expression
- `()` groups expressions together
- `^` matches the start of the string
- `$` matches the end of the string

To learn more about regular expressions and their syntax, you can check out the official Elm documentation or other online resources.

## See Also

- [Official Elm documentation on Regular Expressions](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Learn Regex the Easy Way](https://github.com/ziishaned/learn-regex)
- [Regexone - Learn Regular Expressions with Simple, Interactive Exercises](https://regexone.com/)