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

# Joining Strings in Gleam: A Quick Dive 

## What & Why?
Concatenating strings is simply joining two or more strings together. It's like tying a string of words into a longer sentence, making it easier to display, store or send compound messages in your code.

## How to:
In Gleam, string concatenation is done with the `+` operator.

Here's an example:

```Gleam
let string1 = "Hello, "
let string2 = "world!"
let message = string1 + string2
```

The `message` variable now holds the value `"Hello, world!"`.

## Deep Dive
Historically, string concatenation has existed in most programming languages. In Gleam, it is implemented using an efficient algorithm that prevents excessive memory allocation, ensuring your code runs swiftly.

While the `+` operator is a common method for concatenating strings, you could use `string.append` (from Gleam's `string` module):

```Gleam
let string1 = "Hello, "
let string2 = "world!"
let message = string.append([string1, string2])
```

The `string.append` function takes a list of strings and returns a new string containing all the input strings combined. 

But there's a catch: the `+` operator often performs better for concatenating two strings, while `string.append` shines when dealing with multiple strings.

## See Also 
To explore more on Gleam string manipulations, check out the sources below:
- Gleam's `string` module documentation: [Gleam `string` module](https://hexdocs.pm/gleam_stdlib/gleam/string)
- String operations in Gleam: [Gleam School](https://gleam.run/tour/string/)