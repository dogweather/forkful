---
title:    "Gleam recipe: Capitalizing a string"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered a situation where you needed to capitalize a string in your code? Perhaps you are formatting user input or displaying data in a specific format. Whatever the reason may be, knowing how to capitalize a string in Gleam can come in handy. In this blog post, we will explore how to accomplish this task in a few simple steps.

## How To

To capitalize a string in Gleam, we will be using the `String.capitalize` function. This function takes in a string as its argument and returns a capitalized version of that string.

Let's take a look at an example where we want to capitalize the first letter of a name:

```Gleam
import gleam/String
main =
  let name = "jane"
  String.capitalize(name)
```

The output of this code will be `"Jane"`, with the first letter capitalized. Similarly, we can also capitalize every letter in a string by using the `String.to_uppercase` function:

```Gleam
import gleam/String
main =
  let sentence = "this is a sentence."
  String.to_uppercase(sentence)
```

The output of this code will be `"THIS IS A SENTENCE."` Note that this function does not just capitalize the first letter, but every letter in the string.

## Deep Dive

Behind the scenes, the `String.capitalize` function is using the `String.to_list` function to convert the string into a list of characters. It then uses the `List.head` function to get the first element in the list, which is then mapped to uppercase using the `Char.to_upper` function. Finally, the list is converted back to a string using the `String.from_list` function.

## See Also

- Gleam String module documentation: https://gleam.run/documentation/std-lib/string/
- Capitalizing a string in Elixir: https://elixirschool.com/en/lessons/basics/string/#functions-and-modules