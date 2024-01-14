---
title:                "Elm recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

When working with strings in Elm, one common task is finding the length of a string. This can be useful for a variety of reasons, such as checking the validity of user input or manipulating strings in different ways.

## How To

To find the length of a string in Elm, we can use the `String.length` function. Let's take a look at an example:

```Elm
import String

myString = "Hello, world!"

String.length myString

-- Output: 13
```

In this example, we first import the `String` module which contains the `length` function. We then assign a string to the variable `myString` and pass it as an argument to the `String.length` function. The output is the length of the string, which in this case is 13.

We can also directly use the `length` function without importing the `String` module, as it is a part of the Elm core library:

```Elm
length "Hello, world!"

-- Output: 13
```

## Deep Dive

While the `String.length` function may seem straightforward, there are some intricacies to be aware of. For example, it counts each character in the string, including spaces, punctuation, and special characters. In the first example, the length of the string "Hello, world!" was 13, but if we were to add a space at the end, the length would become 14.

Additionally, the `length` function works with any type of string, whether it be letters, numbers, or even emojis.

```Elm
length "12345"

-- Output: 5

length "😊😊😊"

-- Output: 3
```

It's also important to note that the `length` function is O(n), meaning that the time it takes to find the length of a string increases linearly as the string length increases. This is something to keep in mind when working with longer strings and considering performance.

## See Also

- Official Elm Documentation on `String.length`: https://package.elm-lang.org/packages/elm/core/latest/String#length
- Examples of working with strings in Elm: https://dev.to/jfuentes/working-with-strings-in-elm-1l4d
- A comprehensive guide to becoming an Elm master: https://medium.com/javascript-scene/the-elm-architecture-b4999d07b1d7