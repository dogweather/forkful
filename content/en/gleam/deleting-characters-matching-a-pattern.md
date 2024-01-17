---
title:                "Deleting characters matching a pattern"
html_title:           "Gleam recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters that match a specific pattern is a common task for programmers, as it allows for the manipulation and cleaning of text data. This can be useful for tasks such as data parsing or filtering out unwanted information in a dataset.

## How to:

To delete characters matching a pattern in Gleam, you can use the `String.replace` function along with a regular expression. Here's an example of deleting all vowels from a string:

```Gleam
my_string = "Hello World!"
result = String.replace(my_string, regex"[aeiou]", "")
```

The output will be `"Hll Wrld!"` as all vowels have been removed from the original string. You can also use this method to delete any other characters that match a particular pattern, such as numbers or special characters.

## Deep Dive:

While deleting characters using regular expressions is a common method, it's important to consider the alternative of using pattern matching in Gleam. This can provide more flexibility in terms of matching and manipulating specific parts of a string. However, if you are dealing with large datasets or complex patterns, regular expressions may be a more efficient option.

It's also worth noting that the `String.replace` function in Gleam uses the Rust [`regex`](https://github.com/rust-lang/regex) library under the hood, making it a powerful tool for string manipulation.

## See Also:

- [Gleam Documentation on Regular Expressions](https://gleam.run/documentation/libraries/regular-expressions.html)
- [Rust `regex` Library Documentation](https://docs.rs/regex/)