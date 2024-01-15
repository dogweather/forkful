---
title:                "Finding the length of a string"
html_title:           "Elm recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
Finding the length of a string may seem like a simple task, but it can prove to be quite useful in programming. One reason someone may want to find the length of a string is to validate input data and ensure it meets a specific length requirement.

## How To
To find the length of a string in Elm, we can use the `String.length` function. Here's an example:

```Elm
stringLength : String -> Int
stringLength str =
    String.length str
```

In the above code, we define a function called `stringLength` that takes in a string as an argument and returns an integer representing the length of that string. The `String.length` function is then used to find the length of the given string.

Let's try it out with a sample string:

```Elm
stringLength "Hello World" --> 11
```

As we can see, the string "Hello World" has a length of 11 characters, including the space between "Hello" and "World".

## Deep Dive
Under the hood, the `String.length` function simply iterates through the characters in the string and counts them. However, there are a few things to keep in mind when finding the length of a string in Elm.

Firstly, it's important to remember that in Elm, strings are represented as a list of characters (i.e. `List Char`). This means that finding the length of a string in Elm is essentially the same as finding the length of a list.

Secondly, the `String.length` function is Unicode-aware, meaning it takes into account multi-byte characters when counting the length of a string. This is important to keep in mind for internationalization and handling special characters.

## See Also
- [Elm String Library](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm Basics](https://elmprogramming.com/elmbasics/introduction.html)