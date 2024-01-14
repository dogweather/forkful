---
title:                "Rust recipe: Converting a string to lower case"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

As developers, we often come across situations where we need to manipulate strings in our code. One common task is to convert a string to lower case. By doing so, we can avoid any case sensitivity issues and make our code more robust. In this blog post, we will explore how to easily convert a string to lower case using Rust.

## How To

To convert a string to lower case in Rust, we can use the `to_lowercase()` method. This method is defined on the `String` type and returns a new `String` with all characters converted to lower case. Let's take a look at an example:

```Rust
let my_string = "Hello World".to_string();
let lower_string = my_string.to_lowercase();
println!("{}", lower_string);
```

The output of this code will be `hello world`. As you can see, the `to_lowercase()` method successfully converted all the characters in the string to lower case.

We can also use the `to_lowercase()` method on string literals without needing to convert them to a `String` type:

```Rust
let my_string = "Hello World".to_lowercase();
println!("{}", my_string);
```

The output will still be `hello world` as expected.

## Deep Dive

Under the hood, the `to_lowercase()` method uses the `std::char::ToLowercase` iterator. This iterator performs the actual character-level conversion to lower case. It takes care of any language-specific rules for converting characters, making it a robust and reliable method for all kinds of strings.

It's important to note that the `to_lowercase()` method only works for ASCII characters. If you have a string with non-ASCII characters, you will need to use a different method to convert them to lower case. Rust provides the `unicase::UniCase` type for this purpose, which can handle both ASCII and non-ASCII characters.

## See Also

- [Rust Documentation on Strings and Characters](https://doc.rust-lang.org/std/string/index.html)
- [Rust Cookbook on String Manipulation](https://rust-lang-nursery.github.io/rust-cookbook/text/string_manipulation.html)
- [Official Rust Forum Thread on Converting Strings to Lower Case](https://users.rust-lang.org/t/whats-the-difference-between-matching-and-converting-to-uppercase-lowercase/8393)