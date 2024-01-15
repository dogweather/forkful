---
title:                "Converting a string to lower case"
html_title:           "Rust recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

If you're dealing with text data in your Rust program, you may need to convert a string to lower case for various reasons. This allows for more consistent and easier comparison or manipulation of strings.

## How To

Converting a string to lower case in Rust is a simple process using the `to_lowercase` method. Let's take a look at an example:

```Rust
let my_string = "Hello World";
let lower_string = my_string.to_lowercase();

println!("{}", lower_string); // output: hello world
```

In the above code, we first declare a string variable `my_string` with the value "Hello World". Then, we use the `to_lowercase` method to convert it to lower case and assign the result to a new variable `lower_string`. Finally, we use `println` to print the lower case string to the console.

You can also directly convert a string literal to lower case. Here's an example:

```Rust
let my_string = "I LOVE RUST".to_lowercase();

println!("{}", my_string); // output: i love rust
```

## Deep Dive

Let's take a deeper look at the `to_lowercase` method and how it works behind the scenes. This method is part of the `String` type in Rust and is implemented using the `Unicode` case mapping rules. This means that it takes into account different language-specific characters, accents, and special cases.

Additionally, the `to_lowercase` method returns a new `String` value and does not mutate the original string. This is important to note as it follows Rust's ownership and borrowing rules.

Furthermore, this method is marked as `pub` which means it can be used in your own functions or crates for string manipulation.

## See Also

- [Rust String documentation](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust Unicode documentation](https://doc.rust-lang.org/std/char/trait.ToLowerCase.html)
- [Learn Rust programming language](https://www.rust-lang.org/learn)