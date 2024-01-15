---
title:                "Concatenating strings"
html_title:           "Gleam recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
Strings are a core element in any programming language, allowing us to manipulate and combine text to create dynamic outputs. In Gleam, concatenating strings is a useful tool for creating more complex strings and improving the overall functionality of our code.

## How To
To concatenate strings in Gleam, we use the `++` operator to add two or more strings together. Let's take a look at an example:

```Gleam
let first_name = "John"
let last_name = "Smith"

let full_name = first_name ++ " " ++ last_name

IO.print("Hello " ++ full_name)
```
Output:
```
Hello John Smith
```

In this example, we first declare two string variables, `first_name` and `last_name`. Then, using the `++` operator, we concatenate these two strings with a space in between and assign the result to the `full_name` variable. Finally, we use concatenation once again to add the string "Hello" before the `full_name` variable and print it to the console.

We can also use concatenation with more than just two strings. For example:

```Gleam
let first_name = "John"
let last_name = "Smith"
let age = "25"

let profile = first_name ++ " " ++ last_name ++ ", " ++ age ++ " years old"

IO.print("Profile: " ++ profile)
```
Output:
```
Profile: John Smith, 25 years old
```

In this case, we have concatenated three strings together to create a more detailed profile output.

## Deep Dive
In Gleam, strings are always represented as lists of individual characters. This means that when we use the `++` operator to concatenate strings, we are essentially merging two lists together.

Additionally, it's important to note that the `++` operator is not limited to just strings. We can also concatenate an integer or any other data type with a string. However, we must convert the non-string data into a string using the `to_string()` function first.

## See Also
- [Gleam documentation on strings](https://gleam.run/book/tour/strings.html)
- [Gleam string functions](https://gleam.run/core/String.html)