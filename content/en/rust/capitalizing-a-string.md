---
title:                "Capitalizing a string"
html_title:           "Rust recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string means converting the first character of the string to uppercase and leaving the rest of the characters unchanged. Programmers do this to make strings more visually appealing and consistent, especially when displaying them to users.

## How to:
To capitalize a string in Rust, we can use the built-in `to_uppercase()` function. The function takes in a `String` type and returns a new string with the first character capitalized. Here's an example:
```
let hello = "hello".to_string();
let hello_capitalized = hello.to_uppercase();

println!("{}", hello_capitalized);
```
Output:
```
Hello
```

We can also use the `chars` iterator to manually capitalize the first character of the string. Here's an example:
```
let hello = "hello".to_string();
let mut chars = hello.chars();

if let Some(first_char) = chars.next() {
    println!("{}{}", first_char.to_uppercase(), chars.as_str());
}
```
Output:
```
Hello
```

## Deep Dive:
Capitalizing strings has been a common practice in programming languages, dating back to the early days of programming when punch cards were used. It was typically done to differentiate keywords or commands from regular text. However, with the advances in programming languages and improved user interfaces, capitalization is now mostly used for aesthetic purposes.

There are a few alternatives to the `to_uppercase()` function in Rust, such as the `to_ascii_uppercase()` function, which only capitalizes ASCII characters. There is also the `capitalize` crate, which has additional options for capitalizing strings according to different rules like title casing.

When it comes to implementation, the `to_uppercase()` function uses the `core/unicode/normalization.rs` module in the Rust standard library. It follows the Unicode specification for converting characters to uppercase.

## See Also:
- [Rust Standard Library](https://doc.rust-lang.org/std/index.html)
- [capitalize crate](https://crates.io/crates/capitalize)
- [Unicode Case Mapping Functions](https://www.unicode.org/Public/UNIDATA/CaseFolding.txt)