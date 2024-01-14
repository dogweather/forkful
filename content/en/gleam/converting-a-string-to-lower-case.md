---
title:    "Gleam recipe: Converting a string to lower case"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

In programming, it is often necessary to convert strings to different formats for various purposes. One common conversion is from uppercase to lowercase, which can be useful for data manipulation, input validation, and more.

## How To

To convert a string to lowercase in Gleam, you can use the `String.to_lower_case` function. This function takes in a string as its argument and returns a new string with all letters converted to lowercase.

```
Gleam

import gleam/string

let input = "HELLO WORLD"
let output = String.to_lower_case(input)

```

The output of this code would be `"hello world"`. Keep in mind that the original string remains unchanged and the `String.to_lower_case` function will always return a new string.

You can also use pattern matching to convert individual characters within a string to lowercase. This can be useful for more complex situations where you only want to convert specific characters.

```
Gleam

import gleam/string

let input = "HeLlO wORLd"
let output = input
|> String.chars
|> List.map(\case
  \H -> \h
  \O -> \o
  char -> char
  )
|> String.from_chars
```

In this example, we are using pattern matching within a `List.map` function to convert the letters "H" and "O" to lowercase, while leaving the other characters unchanged. The final output would be `"hello world"`.

## Deep Dive

Understanding how strings are stored in a computer is important for understanding the conversion process. In Gleam, strings are represented as a list of characters under the hood. This means that when we convert a string to lowercase, we are essentially converting each individual character to lowercase and then reconstructing the string with the new characters.

Additionally, the `String.to_lower_case` function uses the Unicode standard for case conversions, which means that it can handle more than just the English alphabet. This is important for internationalization and supports different languages and characters.

## See Also

- [Unicode Standard for Case Conversions](https://unicode.org/standard/standard.html)
- [Gleam Language Documentation](https://gleam.run/documentation/)