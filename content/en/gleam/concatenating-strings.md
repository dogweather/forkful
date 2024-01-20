---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings is the process of joining two or more strings together. Programmers do it to create meaningful messages or to combine data to form usable strings.

## How to:

Here's an example of concatenating strings in Gleam:

```Gleam
let welcome = "Hello, "
let name = "World"
let message = welcome ++ name
```

In this example, `++` is the string concatenation operator, and it combines "Hello, " and "World" into one string "Hello, World".

## Deep Dive

The string concatenation concept has been around since the inception of programming. It's a fundamentally basic operation in most, if not all, programming languages. In Gleam, the `++` operator is used for this operation, influenced by its heritage from the Erlang and Elixir languages.

An alternative to native string concatenation could be string interpolation or the use of functions like `string.append()` depending on what is supported by the language.

Underneath, Gleam's `++` operator is implemented as a list append operator which is pretty standard. It treats strings as lists of characters and appends them together. Resultantly, the time complexity of this operation is linear to the length of the first string which you are concatenating. In other words, concat becomes slower as the first string gets longer.

## See Also:

- Gleam's official documentation on String concatenation: https://gleam.run/book/tour/strings.html 
- Understanding Lists in Erlang (which influences how string concatenation is achieved): http://erlang.org/doc/man/list.html  
- String Interpolation in programming: https://en.wikipedia.org/wiki/String_interpolation