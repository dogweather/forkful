---
title:                "Gleam recipe: Converting a string to lower case"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case is a common task in most programming languages. It is especially useful when dealing with user input or comparing strings in a case-insensitive manner. In Gleam, this can be achieved easily with a built-in function.

## How To

To convert a string to lower case in Gleam, we can use the `String.to_lower` function. Here is a simple example:

```Gleam
import gleam/string

let my_string = "HeLLo WoRlD"
let lower_string = String.to_lower(my_string)

// Output: "hello world"
```

As you can see, the `to_lower` function takes in a string as an argument and returns a new string with all characters converted to lower case. This function can also be used to convert individual characters to lower case, in case you ever need to do so.

## Deep Dive

Under the hood, Gleam's `String.to_lower` function uses the `Unicode.CaseMapping.fold` function to handle all possible unicode characters. This ensures that the conversion is done accurately for all languages and character sets.

Furthermore, the `to_lower` function also takes into account any locale-specific transformations that may be required, making it a robust and reliable way to convert strings to lower case.

## See Also

Here are some other useful links for working with strings in Gleam:

- Official Gleam documentation on strings: https://gleam.run/book/core_string.html
- Unicode support in Gleam: https://gleam.run/book/unicode.html
- Working with locales in Gleam: https://gleam.run/book/locales.html