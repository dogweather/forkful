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

# Deleting Characters Matching a Pattern in Gleam

## What & Why?

Deleting characters matching a pattern in a string is a common programming task, allowing us to cleanse and modify data. This process is key in many scenarios, such as data validation, parsing and transformation.

## How to:

To give you an example, let's take a problem in which we delete all the non-numeric characters from a string. This can be easily done in Gleam using `string.filter_map`.

```Gleam
import gleam/string

pub fn delete_non_numeric_chars(string: String) -> String {
  string
  |> string.to_list
  |> list.filter_map(fn
    | c if string.is_digit(c) -> Some(c)
    | _ -> None
  end)
  |> string.from_list
}

fn main() {
  let modified_string = delete_non_numeric_chars("123aBc456")
  assert modified_string == "123456"
}
```

When you run this, you will receive the output `123456`, as all non-numeric characters are removed.

## Deep Dive

Pattern matching is at the heart of Gleam and many other functional languages, deeply rooted in its history. The method demonstrated above leverages pattern matching to select and delete certain characters, an approach that varies from those like regular expressions, constructs widely used in other languages like Javascript or Python.

A possible alternative to this approach could be using `string.replace`, however it could be more inept for more complicated scenarios. Using `string.filter_map` for pattern matching is more versatile and optimizable for various use-cases.

## See Also:

To learn more about:
- Gleam's pattern matching, check out: https://gleam.run/tour/pattern-matching/
- String functions in Gleam: https://gleam.run/std-lib/gleam/string/ 
- The `filter_map` function: https://gleam.run/std-lib/gleam/list/#filter_map