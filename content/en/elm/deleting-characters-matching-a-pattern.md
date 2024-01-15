---
title:                "Deleting characters matching a pattern"
html_title:           "Elm recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

We've all been there - trying to clean up messy data or unwanted characters from a text input. It can be a tedious and time-consuming task, but luckily Elm has a built-in function that makes it a breeze to delete characters matching a pattern. Whether you're a beginner or experienced programmer, this feature can save you valuable time and effort.

## How To

To delete characters matching a pattern in Elm, we will be using the `String.filter` function. This function takes in two arguments - a Boolean function and a string. It then returns a new string with only the characters that pass the Boolean function.

Let's look at an example. Say we have a string that contains a mix of letters, numbers, and special characters: "H3ll0 W0rld!". And we only want to keep the letters and numbers, discarding the special characters.

We can achieve this by using the `String.filter` function in the following way:

```Elm
String.filter (\c -> Char.isLetter c || Char.isDigit c) "H3ll0 W0rld!"
```

The `\c` is an anonymous function that takes in a character `c` and checks if it is either a letter or a digit, using the `Char.isLetter` and `Char.isDigit` functions. We then pass in the original string as the second argument to the `String.filter` function.

The output of this code will be a new string with only the letters and numbers: "H3ll0W0rld".

## Deep Dive

The `String.filter` function is a flexible tool that can be used in various scenarios. It allows you to specify a custom Boolean function to filter out the desired characters. This function can be as simple or complex as you need it to be, making it a powerful tool in data cleaning and manipulation.

It's worth noting that the `String.filter` function does not modify the original string, but instead returns a new string with the filtered characters. This is because in Elm, strings are immutable, meaning they cannot be changed in-place. Instead, new strings are created when modifications are made.

## See Also

- Elm String.filter documentation: https://package.elm-lang.org/packages/elm/core/latest/String#filter
- Char.isLetter documentation: https://package.elm-lang.org/packages/elm/core/latest/Char#isLetter
- Char.isDigit documentation: https://package.elm-lang.org/packages/elm/core/latest/Char#isDigit