---
title:                "Interpolating a string"
html_title:           "Gleam recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Interpolating a string is a technique used by programmers to insert variables or values into a string dynamically, without having to concatenate multiple strings together. This improves the readability and efficiency of the code.

## How to:

Interpolation is done in Gleam by using the `format` function and using curly braces `{}` around the variables or values that need to be inserted. Here's a simple example:

```Gleam
let name = "Jane"
let message = format("Hello, {}", name)
```

The `message` variable now contains the string "Hello, Jane". We can also interpolate multiple variables or values, and format them as desired. Here's another example:

```Gleam
let age = 25
let message = format("I am {} years old and my name is {}", age, name)
```

The `message` variable now contains the string "I am 25 years old and my name is Jane". 

## Deep Dive:

Interpolating strings has been around for a long time and is commonly used in many programming languages. It helps in avoiding repetitive concatenation of strings and also makes the code more concise and readable. 

There are alternative methods of string interpolation, such as using string concatenation or format specifiers, but they are not as efficient as using the `format` function in Gleam. Using concatenation can result in complex and unreadable code, while format specifiers may not offer the degree of flexibility and customization that Gleam's `format` function provides.

Under the hood, the `format` function uses string formatting syntax from the [RFC 5424](https://tools.ietf.org/html/rfc5424) standard, allowing for a wide range of formatting options. This makes it a powerful tool for handling strings in a concise and efficient manner.

## See Also:

- [Gleam's Official Documentation](https://gleam.run/documentation)
- [String Interpolation in Other Programming Languages](https://en.wikipedia.org/wiki/String_interpolation#In_other_languages)