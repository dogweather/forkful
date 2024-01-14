---
title:                "Rust recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why Capitalize a String in Rust

When working with strings in Rust, there may be situations where you need to capitalize a string, either for aesthetic purposes or to adhere to a specific output format. Luckily, Rust provides an easy and efficient way to capitalize a string, making it a valuable tool for any Rust programmer.

## How To Capitalize a String in Rust

To capitalize a string in Rust, we will be using the `to_uppercase()` method from the `String` class. This method converts all characters in a string to uppercase, effectively capitalizing it. Let's take a look at an example:

```rust
let name = "john";
let capitalized_name = name.to_uppercase();
println!("Original name: {}", name); // prints "john"
println!("Capitalized name: {}", capitalized_name); // prints "JOHN"
```

In this example, we create a string variable called `name` and assign it the value "john". We then use the `to_uppercase()` method to convert the string to uppercase and assign the result to a new variable `capitalized_name`. Finally, we print out both the original and capitalized versions of the string to see the difference.

This method can also be applied to string variables that are user input or read from external sources. Here's an example of capitalizing a user input:

```rust
use std::io;
let mut input = String::new();
println!("Enter a word:");
io::stdin().read_line(&mut input).expect("Failed to read input");
let capitalized_input = input.to_uppercase();
println!("Capitalized input: {}", capitalized_input); // prints user input in all uppercase
```

In the above code, we use the `std::io` module to read a user input and store it in the `input` variable. We then use the `to_uppercase()` method to capitalize the input and print it out.

## Deep Dive into Capitalizing a String in Rust

Behind the scenes, the `to_uppercase()` method in Rust utilizes the Unicode Standard's case conversion rules. This means that it can correctly handle characters from different languages and scripts.

Additionally, this method is available not just for the `String` class, but also for the `&str`, `&mut str`, and `StringView` types, making it a versatile tool for any situation.

See Also
- Rust Strings: https://doc.rust-lang.org/std/string/struct.String.html
- Rust Case Conversion Rules: https://unicode.org/reports/tr21/tr21-5.html#To_Lowercase_and_Uppercase