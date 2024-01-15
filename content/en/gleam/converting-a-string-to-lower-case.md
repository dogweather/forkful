---
title:                "Converting a string to lower case"
html_title:           "Gleam recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case is a fundamental task in text processing. It allows for easier comparison and manipulation of strings, which is essential in many programming applications.

## How To

To convert a string to lower case in Gleam, you can use the built-in function `String.to_lower` as shown in the following code block:

```Gleam {linenos=table}
// Define an example string
let str = "Hello World"

// Convert the string to lower case
let lower = String.to_lower(str)

// Print the result
// Output: hello world
io.println(lower)
```

The `String.to_lower` function takes in a string as its argument and returns a new string with all letters converted to lower case. In the code above, we first define an example string "Hello World" and then use the `to_lower` function to convert it. The result is then printed using the `io.println` function.

You can also use a string pattern matching to convert multiple strings to lower case at once, as shown in the following code block:

```Gleam {linenos=table}
// Define a list of strings
let strings = ["Gleam", "Programming", "Is", "Fun"]

// Convert all strings to lower case using pattern matching
let lower_strings = for string in strings {
  lower -> String.to_lower(string)
}

// Print the result
// Output: ["gleam", "programming", "is", "fun"]
io.println(lower_strings)
```

In the code above, we use pattern matching to iterate through the list of strings and convert each one to lower case. The `for` loop returns a new list with all the lower case strings, which is then printed using the `io.println` function.

## Deep Dive
Gleam's `String.to_lower` function uses the Unicode standard to handle characters that have different cases, such as ü and Ü. It is also optimized for performance, making it a reliable choice for converting strings to lower case in large-scale text processing.

You can also utilize external libraries, such as `gunicode`, for more advanced handling of characters and cases in your conversions. These libraries provide additional functions and utilities for working with Unicode characters and text in Gleam.

## See Also
- [Gleam Standard Library](https://gleam.run/lib)
- [Gleam by Example](https://github.com/gleam-lang/gleam_by_example)
- [gunicode Library for Gleam](https://github.com/gleam-lang/gunicode)