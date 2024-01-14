---
title:                "Gleam recipe: Capitalizing a string"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why Capitalize a String in Gleam

Capitalizing a string is a common task in programming and can be useful for various reasons. For example, when displaying user input or creating proper titles, it is important to have the first letter of each word capitalized. In this blog post, we will explore how to capitalize a string in Gleam and provide a deep dive into the process.

## How To Capitalize a String in Gleam

To capitalize a string in Gleam, we will use the `String.to_title_case` function. This function takes in a string as its parameter and returns the capitalized string. Let's take a look at an example:

```Gleam
import String
import Gleam.List

let name = "john smith"

let capitalized_name = String.to_title_case(name)

Gleam.List
  .map(String.to_list(capitalized_name), char_to_list)
  # Output: ['J', 'o', 'h', 'n', ' ', 'S', 'm', 'i', 't', 'h']
```

As we can see from the output, `String.to_title_case` has successfully capitalized the first letter of each word in our string. It is important to note that this function will not alter any other letters in the string - only the first letter of each word will be capitalized.

## Deep Dive into Capitalizing a String

Behind the scenes, the `String.to_title_case` function uses Unicode rules to determine which letters should be capitalized. This means that it can handle a variety of languages and special characters, making it a robust solution for capitalizing strings.

It is also worth noting that the function works with not only single words, but with whole sentences as well. Each word in the sentence will be capitalized, while any other punctuation or symbols will remain unchanged. Additionally, if the string contains any non-letter characters, they will be preserved in the output.

## See Also

- [Gleam Documentation on String](https://gleam.run/documentation/stdlib/string/)
- [Unicode Character Database](https://unicode.org/ucd/)
- [Gleam Discord Community](https://discord.gg/XzYcS2X)