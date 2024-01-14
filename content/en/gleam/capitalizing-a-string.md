---
title:                "Gleam recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing a string is a common task in programming, especially when working with user input or formatting data for display. It can help improve the readability and consistency of your code, making it easier to understand for both you and others.

## How To

To capitalize a string in Gleam, we can use the `String.capitalise` function. Let’s take a look at an example:

```Gleam
import gleam/string

let my_string = "hello world"
let capitalized_string = string.capitalise(my_string)

IO.println(capitalized_string)
```

This will output `Hello world`. As you can see, the first letter of the string has been capitalized while the rest of the letters remain unchanged.

We can also capitalize all words in a string by using the `String.capitalise_all_words` function. For example:

```Gleam
let my_string = "hello world"
let capitalized_string = string.capitalise_all_words(my_string)

IO.println(capitalized_string)
```

This will output `Hello World`. In addition, we can also specify a particular language for capitalization rules by passing a `lang` argument to these functions.

## Deep Dive

Under the hood, the `String.capitalise` and `String.capitalise_all_words` functions use the `String.foldl` function. This allows us to customize how the string is capitalized by defining our own rules.

For example, if we want to capitalize the first letter of every word and convert all remaining letters to lowercase, we can do so by passing in a custom function to the `String.foldl` function:

```Gleam
fn update_character(_index, character, accumulator) {
  case accumulator {
    _ -> character
    | ' ' -> Unicode(Unicode.byte_to_uppercase(character))
  }
}

let my_string = "hello world"
let capitalized_string = string.foldl(update_character, my_string)

IO.println(capitalized_string)
```

This will output `Hello World`. By understanding the underlying functions, we have more control and flexibility when it comes to capitalizing strings in Gleam.

## See Also

- Official Gleam Documentation for `String` module: https://gleam.run/modules/string.html
- Tutorial for learning Gleam: https://gleam.run/book/tour.html
- Further reading on working with strings in Gleam: https://stratus3d.com/blog/2020/02/20/working-with-strings-in-gleam/