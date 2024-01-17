---
title:                "Capitalizing a string"
html_title:           "Gleam recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string means changing the first letter of each word in the string to its uppercase form. Programmers often do this for consistency, readability, and stylistic purposes. For example, in English, the titles of books, documents, or variable names commonly follow this convention.

## How to:
To capitalize a string in Gleam, use the `String.capitalize_words` function. Here is an example:

```Gleam
let string = "hello world"
let capitalized_string = String.capitalize_words(string)

// Output: "Hello World"
``` 

The `capitalize_words` function takes in a string as an argument and returns a new string with the first letter of each word capitalized.

You can also use the `capitalize` function if you only want to capitalize the first letter of the string. Here's an example:

```Gleam
let string = "hello world"
let capitalized_string = String.capitalize(string)

// Output: "Hello world"
```

## Deep Dive
Capitalization in programming has roots in the English language, where uppercase letters were used for emphasis, headings, and titles. This convention eventually made its way into programming languages, where it serves both functional and aesthetic purposes.

Some alternative ways to capitalize a string include using regular expressions or manually looping through and capitalizing each letter. However, these methods are more verbose and not always the most efficient.

Internally, the `capitalize_words` function in Gleam converts the string to a list of words, capitalizes the first letters, and then joins them back together to form a new string. This process ensures that only the first letter of each word is capitalized, and the rest of the string remains as it is.

## See Also
To learn more about string manipulation in Gleam, check out the official documentation on string operations: <https://gleam.run/documentation/stdlib/string/#operations>

For a deeper dive into the history of capitalization in programming, this article by the Association for Computing Machinery is a great resource: <https://cacm.acm.org/news/210703-camel-case/history>