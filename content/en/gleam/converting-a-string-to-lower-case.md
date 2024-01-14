---
title:                "Gleam recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case is a common task in programming, especially when dealing with user inputs or comparing strings for equality. By converting all characters to lower case, we eliminate the issue of case sensitivity and make it easier to handle strings in our code.

## How To

To convert a string to lower case in Gleam, we can use the `string.to_lower` function. Let's take a look at an example:

```Gleam
let name = "John"
let lower_case_name = string.to_lower(name)
```

In this code, we first declare a string variable `name` with the value of "John". Then, we use the `string.to_lower` function to convert the string to lower case and assign the result to a new variable `lower_case_name`. The output of this example would be "john".

We can also use this function to directly convert user inputs to lower case. For example:

```Gleam
let input = IO.read_line()
let lower_case_input = string.to_lower(input)
```

Here, we use the `IO.read_line()` function to prompt the user for input, which we then convert to lower case using `string.to_lower` and assign to the `lower_case_input` variable.

## Deep Dive

In Gleam, strings are represented as `string` terms, which are actually lists of Unicode code points. When we use the `string.to_lower` function, it iterates through each code point in the string and converts any uppercase letters to their lowercase equivalents.

One thing to note is that this function only works with Unicode code points, so if you are dealing with non-Unicode characters, you may need to use a different approach.

Another useful function when working with strings in Gleam is `string.to_upper`, which does the opposite of `string.to_lower` and converts all characters to uppercase.

## See Also

- Gleam Documentation: https://gleam.run/documentation/
- Learn Gleam - Basics: https://gleam.run/book/basics
- String Manipulation in Gleam: https://gleam.run/book/strings