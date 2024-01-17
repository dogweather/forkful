---
title:                "Using regular expressions"
html_title:           "Gleam recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Regular expressions are a useful tool for programmers to search, extract, and manipulate text based on patterns. They are used in programming languages, text editors, and other programs to perform complex text processing tasks. Programmers use regular expressions to save time and effort by automating tasks that would otherwise be tedious to do manually.

## How to:

Using regular expressions in Gleam is simple and efficient. Here are a few examples to help you get started:

```
Gleam
let email_regex = Regex.new("^([a-z0-9_\.-]+)@([\da-z\.-]+)\.([a-z\.]{2,6})$")

let email = "example@email.com"

match email_regex.match(email) {
  | Ok(match) -> match.string
  // Output: "example@email.com"
  | Error(_) -> "Invalid email"
}

let sentence_regex = Regex.new("([a-z]+) ([0-9]+)")

let text = "I have 3 cats and 2 dogs"

text
|> sentence_regex.matches
|> List.filter(_, Fn.and(is_match(_), Fn.compose(is_even, count)))
|> List.map(_, Fn.compose(get_value(_, 1), get_values))

//Output: [ "have", "and" ]
```

## Deep Dive:

Regular expressions have a long history dating back to the 1950s. They were originally developed to simplify pattern matching in the field of computer science. Today, they are widely used in various programming languages and tools, such as Perl, Python, and Vim.

Alternatives to regular expressions include string manipulation using built-in functions or using other libraries specifically designed for text processing. However, regular expressions remain a popular choice due to their flexibility and powerful pattern matching capabilities.

In Gleam, regular expressions are implemented using the Rust library, regex. This allows for efficient and reliable pattern matching, making it a robust tool for programmers.

## See Also:

To learn more about regular expressions, check out the official Rust regex library documentation [here](https://docs.rs/regex/1.4.3/regex/). You can also explore other useful resources such as [Mastering Regular Expressions](http://regex.info/) by Jeffrey Friedl and [Regular-Expressions.info](https://www.regular-expressions.info/) by Jan Goyvaerts.