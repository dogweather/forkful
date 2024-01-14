---
title:    "Elm recipe: Finding the length of a string"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

As programmers, we often need to manipulate strings in our code. One common task is finding the length of a string. This may seem like a simple task, but it can have a big impact on the overall functionality and efficiency of our program. That's why it's important to understand how to find the length of a string in Elm.

## How To

To find the length of a string in Elm, we can use the `String.length` function. Here's an example code block showing how to use this function:

```Elm
import String exposing (length)

string = "Hello World!"
length string
```

The output of this code block would be `12`, as `"Hello World!"` has 12 characters. It's important to note that special characters, such as accented letters or emojis, are also counted as characters in the string.

Another way to find the length of a string is by using the built-in function `String.toList` and then finding the length of the list. Here's an example:

```Elm
import String exposing (toList)

string = "Elm is fun! 🚀"
toList string |> List.length
```

This would also output `12`. However, this method may not be as efficient as using `String.length`, especially for longer strings.

## Deep Dive

When we call the `String.length` function, Elm actually runs through each character in the string to count them. This may seem like a lot of work, but it's actually a very efficient process. The efficiency is due to the fact that Elm strings are represented as a sequence of Unicode code points, which allows for fast indexing and counting.

In addition, `String.length` also takes into account multi-byte characters, making it a more reliable method for finding the length of a string compared to other programming languages.

## See Also

Now that you know how to find the length of a string in Elm, you can further explore the various string functions available in the Elm standard library. Here are some helpful links:

- [Elm String Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm Cheat Sheet: String Functions](https://www.lucify.com/blog/elm-cheat-sheet-string-functions/)
- [Elm Tutorials: Strings](https://elmprogramming.com/strings.html)