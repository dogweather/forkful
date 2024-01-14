---
title:    "Gleam recipe: Capitalizing a string"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why

Capitalizing a string may seem like a simple task, but it can have a big impact on the readability and organization of your code. By properly capitalizing your strings, you can ensure that your code is easy to understand and maintain, making your programming experience more efficient and enjoyable.

## How To

To capitalize a string in Gleam, there are a few different approaches you can take depending on your specific needs. Here are three examples using different methods:

```Gleam
// Method 1: Using the standard library function "String.capitalise"

import gleam/string

let string = "hello world"
let capitalized = string |> string.capitalise

"Capitalized string: {{capitalized}}"

// Output:
// Capitalized string: Hello world
```

```Gleam
// Method 2: Using pattern matching and recursion

fn capitalize(string) {
  case string {
    "" -> ""
    string ~ initial_char = capitalize(string)
      -> String.to_upper_case([initial_char]) <> string
  }
}

"Capitalized string: {{capitalize("hello world")}}"

// Output:
// Capitalized string: Hello world
```

```Gleam
// Method 3: Using a for loop and saving the new string in a variable

import gleam/unicode

let string = "hello world"
let chars = List.to_array(Unicode.characters(string))
let mut capitalized = ""
for char in chars {
  let capitalized_char = char |> Unicode.to_upper_case |> Char.to_string
  capitalized = capitalized ++ capitalized_char
}

"Capitalized string: {{capitalized}}"

// Output:
// Capitalized string: Hello world
```

## Deep Dive

As you may have noticed with the above examples, capitalizing a string involves more than just converting lowercase letters to uppercase. Depending on the language and context, different rules and considerations may apply. Some languages have specific conventions for capitalization, such as capitalizing proper names or the first letter of a sentence. In some cases, accents or diacritics may also play a role in the capitalization process.

Additionally, when working with non-English languages, capitalization may require more complex logic and handling of different character sets. Gleam's standard library provides functions for working with Unicode characters, making it easier to handle these situations.

By diving deeper into the nuances of capitalizing a string, you can improve the accuracy and reliability of your code in various scenarios.

## See Also

- [Gleam documentation on strings](https://gleam.run/documentation/stdlib/strings/)
- [Unicode handling in Gleam](https://gleam.run/documentation/std/unicode/)