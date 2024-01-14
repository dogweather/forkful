---
title:                "Elm recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why
As a programmer, there may come a time when you need to manipulate strings in your code. One useful operation to know is extracting substrings, which allows you to retrieve a portion of a string based on a given start and end index. This can come in handy when working with user input or when parsing data from various sources.

## How To
To extract substrings in Elm, we can use the `String.slice` function. This function takes in three arguments: the original string, the starting index, and the ending index. Let's take a look at an example:

```
```Elm
partialString : String
partialString = String.slice "Hello World!" 6 11
-- Output: "World"
```

In this example, we have a string "Hello World!" and we want to extract a portion of it starting at index 6 (which is the letter "W") and ending at index 11 (which is the exclamation mark). The result is the substring "World", which is assigned to the variable `partialString`.

We can also use negative indices to extract substrings from the end of a string. For example:

```
```Elm
partialString : String
partialString = String.slice "Hello World!" -1 -3
-- Output: "Hello World"
```

In this case, we want to extract a substring starting at the last character and ending at the third to last character, resulting in the full string "Hello World!".

## Deep Dive
When extracting substrings, it's important to keep in mind that the second index (the ending index) is non-inclusive. This means that the substring will not include the character at the ending index. For example, if we use indices 6 and 11 as in our first example, the resulting substring will include the characters at indices 6 to 10, but not index 11.

We can also use the `String.length` function to determine the length of a string, which can be useful when calculating indices for extracting substrings. For example, we can extract the first five characters of a string by using an ending index of `String.length string`.

```
```Elm
partialString : String
partialString = String.slice "Hello World!" 0 (String.length "Hello World!" - 6)
-- Output: "Hello"
```

In this example, we are extracting a substring starting at index 0 and ending six characters before the end of the string, resulting in the first five characters "Hello".

## See Also
For more information on extracting substrings in Elm, you can check out the official documentation for the `String.slice` function: https://package.elm-lang.org/packages/elm/core/latest/String#slice

You can also see the Elm Guide section on Strings for more string manipulation operations: https://guide.elm-lang.org/strings/

For a deeper dive into strings and other core language concepts in Elm, you can explore the official Elm Guide: https://guide.elm-lang.org/core_language.html

Happy coding!