---
title:                "Gleam recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to extract specific parts of a string in your code? Whether it's for data manipulation or formatting purposes, substring extraction is a common task in programming. In Gleam, there are built-in functions that make this process quick and easy. In this blog post, we'll explore how to extract substrings in Gleam.

## How To

Coding in Gleam is easy and follows a functional programming style. To extract substrings, we'll be using the `String` and `List` modules. Let's take a look at a simple example:

```
Gleam> import String
Gleam> import List
Gleam> let string = "Hello World!"
Gleam> let substring = String.slice(string, 6, 11)
Gleam> IO.print(substring)
"World"
```

In the code above, we first import the `String` and `List` modules to access their functions. Then, we assign our string to a variable called `string`. We use the `String.slice` function and provide the starting and ending index of the substring we want to extract. In this case, we want to extract from the 6th to the 11th character, which is "World". Lastly, we use the `IO.print` function to display the extracted substring.

But what if we want to extract multiple substrings from a string? That's where the `String.slice_many` function comes in. Let's see it in action:

```
Gleam> import String
Gleam> import List
Gleam> let string = "This is a sentence"
Gleam> let substrings = String.slice_many(string, [5, 8])
Gleam> IO.print(substrings)
[ "is", "a" ]
```

In this example, we use the `String.slice_many` function and provide a list of indices where we want to extract substrings. This is useful for extracting specific words or phrases from a string.

## Deep Dive

Under the hood, the `String.slice` and `String.slice_many` functions use the `List.take` and `List.drop` functions from the `List` module. This allows for more flexibility in manipulating the extracted substrings. For example, we can reverse a substring by using the `List.reverse` function:

```
Gleam> import String
Gleam> import List
Gleam> let string = "Gleam is awesome"
Gleam> let substring = String.slice(string, 0, 5) |> List.to_array |> List.reverse |> List.to_string
Gleam> IO.print(substring)
"maelG"
```

In the code above, we extract the first 5 characters from the string, convert it to an array, reverse it, and convert it back to a string. This gives us a reversed substring.

## See Also

- Gleam Documentation: https://gleam.run/
- Official Gleam GitHub Repository: https://github.com/gleam-lang/gleam