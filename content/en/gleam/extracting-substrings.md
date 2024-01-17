---
title:                "Extracting substrings"
html_title:           "Gleam recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings is the act of retrieving a smaller piece of text from a larger string. This is a common task in programming, especially when working with user input or manipulating data. By using substrings, programmers can easily access and manipulate specific portions of text without having to process the entire string.

## How to:

To extract substrings in Gleam, we can use the `String.slice` function. This function takes in the string we want to extract from, as well as the start and end index of the desired substring. For example, if we wanted to extract the three middle characters from the string "Hello World", we could use the following code:

```
Gleam> String.slice("Hello World", 4, 7)
```

This would give us the output of "lo ". It's important to note that the start index is inclusive, while the end index is exclusive. This means that the character at the end index is not included in the final substring.

## Deep Dive:

The concept of extracting substrings has been around for a long time and is a fundamental part of string manipulation in programming. Most programming languages have built-in functions or libraries for extracting substrings, and Gleam is no exception.

However, there are alternative techniques for extracting substrings, such as using regex or looping through the string and extracting characters manually. It's important for programmers to understand the different options and choose the one that best fits their specific use case.

In Gleam, the `String.slice` function works by utilizing the Erlang `binary:part` function under the hood. This allows for efficient substring extraction without having to allocate any new memory. Additionally, Gleam also provides the `String.slice_to_end` function for extracting the remaining portion of a string starting from a given index.

## See Also:

- [Gleam String module documentation](https://gleam.run/docs/stdlib/string/)
- [Erlang binary module documentation](http://erlang.org/doc/man/binary.html)
- [Alternative techniques for extracting substrings in Java](https://www.baeldung.com/string/substrings-java)