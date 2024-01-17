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

## What & Why?

Finding the length of a string is a common task in programming, especially when dealing with user input or manipulating text data. It simply means calculating the number of characters in a string. This is important because it allows programmers to perform operations like slicing and indexing on strings.

## How to:

To find the length of a string in Elm, we can use the `String.length` function. This function takes in a string as its argument and returns the number of characters in the string. Here's an example of using the function:

```Elm
myString = "Hello World"
stringLength = String.length myString
```

The variable `stringLength` will now hold the value `11`, which is the length of the string "Hello World".

## Deep Dive:

The concept of finding the length of a string may seem trivial, but it has a rich history in computer science. The most commonly used algorithm for string length calculation is the `strlen` function in the C programming language, which traverses the entire string and counts the number of characters until it reaches a null terminator. In Elm, this is achieved through the use of Unicode codepoints to handle multi-byte characters.

There are also other ways to find the length of a string, such as using regular expressions or calculating the length based on the number of bytes. However, these methods may not always be accurate or efficient.

## See Also:

To learn more about strings in Elm, you can check out the official Elm Guide on strings: https://guide.elm-lang.org/strings/.

You may also be interested in exploring other string manipulation functions in Elm, such as `String.slice` and `String.index`, which allow you to extract specific parts of a string.

That's it for finding the length of a string in Elm. Happy coding!