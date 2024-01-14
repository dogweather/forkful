---
title:                "Gleam recipe: Concatenating strings"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
String concatenation is a fundamental and commonly used operation in programming, especially when working with text data. It allows us to combine multiple strings into one, making it easier to manipulate and display data in a desired format. In this blog post, we will explore how to concatenate strings in the Gleam programming language.

## How To
To concatenate strings in Gleam, we can use the `<>` operator or the `String.concat` function. Let's see some examples:

```Gleam
let first_name = "John"
let last_name = "Smith"

// Using the <> operator
let full_name = first_name <> " " <> last_name
// Output: "John Smith"

// Using the String.concat function
let full_name = String.concat([first_name, " ", last_name])
// Output: "John Smith"
```

We can also use string interpolation to concatenate strings with other data types, such as integers or booleans:

```Gleam
let age = 30
let intro = "I am " <> age <> " years old."
// Output: "I am 30 years old."

let married = true
let status = "I am currently " <> if married { "married" } else { "single" }
// Output: "I am currently married"
```

It's important to note that string concatenation in Gleam always creates a new string and does not mutate the original strings.

## Deep Dive
Behind the scenes, string concatenation in Gleam is implemented using an efficient data structure known as a rope. A rope consists of small chunks of text that are linked together, allowing for fast concatenation of strings without the need for creating new copies of the entire string. This is especially useful when dealing with large amounts of text data.

In Gleam, strings are also represented as Unicode code points, allowing for robust handling of different character sets and languages. This ensures that string concatenation works seamlessly regardless of the language used.

## See Also
For more information on strings and other data types in Gleam, check out the official documentation and tutorials:
- [Gleam Documentation](https://gleam.run/documentation/)
- [Getting Started with Gleam](https://gleam.run/getting-started/)
- [Working with Strings in Gleam](https://gleam.run/fundamentals/strings/)

Happy coding with Gleam!