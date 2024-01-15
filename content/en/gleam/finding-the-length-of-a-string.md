---
title:                "Finding the length of a string"
html_title:           "Gleam recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why?

The length of a string is a fundamental concept in programming and is required for a variety of tasks such as input validation, data manipulation, and error handling. Understanding how to find the length of a string is an essential skill for any programmer.

## How To

Finding the length of a string in Gleam is a simple process that can be achieved using the built-in `String.length` function. Let's take a look at an example:

```Gleam
import gleam/string

let name = "John"

let len = String.length(name)

debug!{"The length of the string is {len}"} // Output: The length of the string is 4
```

In this example, we first import the `string` module from the Gleam standard library. Then, we create a string variable named `name` and assign it a value of "John". Next, we use the `String.length` function to find the length of the string and store it in a variable called `len`. Finally, we use the `debug!` function to print out the length of the string to the console.

## Deep Dive

Behind the scenes, the `String.length` function uses the code unit count to determine the length of a string. A code unit is an element within a string that represents a single character. In languages like Gleam, each code unit is typically represented by one Unicode code point, which is a unique number assigned to each character in the Unicode standard.

It's important to note that the length of a string can differ from the number of characters it contains, as some characters may require more than one code unit. For example, the character "é" requires two code units to represent it in Unicode.

## See Also

- [Gleam documentation on string manipulation](https://gleam.run/documentation/std-lib/string/)
- [Unicode code points](https://en.wikipedia.org/wiki/Unicode)
- [The difference between code units and characters](https://en.wikipedia.org/wiki/Language_localisation#Word_size)