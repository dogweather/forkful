---
title:    "Elm recipe: Converting a string to lower case"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting strings to lowercase is a common task in programming, especially when working with user input or data manipulation. In Elm, this can be achieved using a built-in function, making it a quick and easy solution for developers.

## How To

To convert a string to lowercase in Elm, we can use the `String.toLower` function. This function takes in a string as its parameter and returns a new string with all characters converted to lowercase. Let's take a look at an example:

```elm
name = "ELM PROGRAMMING"
lowercaseName = String.toLower name

-- Output: lowercaseName = "elm programming"
```

In this example, we have a string `name` with all capital letters. By using `String.toLower`, we are able to convert it to lowercase and assign it to the `lowercaseName` variable. We can then use this variable in our code further.

Another useful function is `String.toUpper`, which does the opposite and converts all characters to uppercase. These functions are useful when dealing with case-sensitive data or when we want to normalize user inputs.

## Deep Dive

Under the surface, the `String.toLower` function works by using the `Char.toLower` function on each character of the string. This function takes in a character and returns a lowercase version of it, or the original character if it is already lowercase.

One thing to keep in mind is that these functions only work for English characters. If you are working with non-English characters, you might need to use a different library or function to handle case conversions.

## See Also

For more information on string operations in Elm, you can refer to the official Elm documentation or check out these resources:

- [The Basics of Strings in Elm](https://guide.elm-lang.org/appendix/strings.html)
- [Elm String Library](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm Strings Cheat Sheet](https://devhints.io/elm-strings)