---
title:                "Elm recipe: Finding the length of a string"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why 

Finding the length of a string may seem like a trivial task, but it is a crucial part of many programming tasks. In Elm, strings are an important data type that are commonly used to store and manipulate text. By being able to determine the length of a string, you can perform various operations like substring extraction, validation, and more. 

## How To

To find the length of a string in Elm, we can use the built-in `String.length` function. Here's an example of how you can use it: 

```elm
import String exposing (length)

myString = "Hello, world!"
lengthOfMyString = length myString

-- Output: 13
```

In this code, we first import the `String` module, specifically the `length` function. Then, we declare a string variable called `myString` with the value "Hello, world!". Finally, we call the `length` function with `myString` as the argument, which returns the length of the string and assigns it to `lengthOfMyString`.

Another way to find the length of a string is by using the `String.length` function with the `String.fromList` function. Here's an example: 

```elm
import String exposing (length, fromList)

myList = ['H', 'e', 'l', 'l', 'o']
lengthOfMyList = length (fromList myList)

-- Output: 5
```

In this code, we first import both the `String` module and the `fromList` function. Then, we declare a list of characters called `myList` with the value "Hello". We then use the `fromList` function to convert the list to a string and pass it as an argument to the `length` function.

## Deep Dive 

Behind the scenes, the `String.length` function uses the `String.codePointOffset` function to determine the length of a string. This function counts the number of code points in a string, which is more accurate than simply counting the characters. This is because some characters, like emojis or special characters, can be represented by multiple code points. For example, the emoji "👋" is actually represented by two code points. Knowing this, we can see that the length of "👋" is 2, not 1.

## See Also 

- Official Elm documentation for string functions: https://package.elm-lang.org/packages/elm-lang/core/latest/String
- Tutorial on manipulating strings in Elm: https://guide.elm-lang.org/appendix/strings.html
- Helpful guide on code points in Elm: https://discourse.elm-lang.org/t/declaring-a-code-point/3384