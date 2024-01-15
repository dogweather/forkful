---
title:                "Extracting substrings"
html_title:           "Elm recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to extract a specific piece of text from a larger string? Maybe you're working with user input or parsing data from an API response. Elm has a built-in function for extracting substrings, making it a useful tool for manipulating text in your programs.

## How To

To extract a substring in Elm, we use the `String.slice` function. This function takes two arguments: the starting index and the ending index of the desired substring. Here's an example of extracting a substring from a string called `message`:

```Elm
message = "Hello World"
substring = String.slice 0 5 message
```

In this example, we're extracting the first 5 characters from the string `message`, resulting in the substring "Hello". Keep in mind that the first index is inclusive and the second index is exclusive, meaning the character at the ending index will not be included in the extracted substring.

We can also use negative numbers for the indexes, which will count backwards from the end of the string. For example, if we wanted to extract the last 5 characters from the string `message`, we could use the following code:

```Elm
message = "Hello World"
substring = String.slice -5 0 message
```

This would result in the substring "World".

It's also important to handle edge cases, such as when the starting index is beyond the end of the string. In this case, the function will simply return an empty string. For example:

```Elm
message = "Hello World"
substring = String.slice 20 25 message -- returns an empty string
```

## Deep Dive 

Behind the scenes, the `String.slice` function uses the `String.substring` function, which takes the same arguments but is slightly more low-level. The main difference is that the ending index in `String.substring` is inclusive, meaning the character at that index will be included in the extracted substring. In most cases, you'll want to use `String.slice` for extracting substrings.

There are also two other functions for extracting substrings in Elm: `String.left` and `String.right`, which take a single argument representing the number of characters to extract from the beginning or end of a string, respectively.

## See Also 

- [Elm String module](https://package.elm-lang.org/packages/elm/core/latest/String)
- [MDN String.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)