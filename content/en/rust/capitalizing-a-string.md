---
title:    "Rust recipe: Capitalizing a string"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

String manipulation is a common task in programming, and one task that often comes up is capitalizing a string. Whether you're trying to format user input or make your text stand out, capitalizing a string can be a useful tool in your arsenal. In this blog post, we'll explore how to capitalize a string using Rust.

## How To

Let's start by creating a simple Rust program that takes in a string from the user and capitalizes it. We'll first need to import the `text_io` crate, which allows us to read user input. Then, we'll use the `to_uppercase` method to capitalize the string:

```Rust
// Import the text_io crate
use text_io::read;

// Prompt user for input 
println!("Enter a string: ");

// Read user input
let mut input = read!("{}\n");

// Capitalize the string
let capitalized = input.to_uppercase();

// Print out the capitalized string
println!("Capitalized string: {}", capitalized);
```

Let's run this code and see what happens. If we input "hello world," the program will output "HELLO WORLD." 

## Deep Dive

Now, let's dive a little deeper into the `to_uppercase` method. This method uses the current locale to perform the conversion to uppercase. This means that the output may differ depending on the user's operating system or system settings. 

Additionally, the `to_uppercase` method is not UTF-8 aware, so it may not properly handle non-ASCII characters. In these cases, it's better to use the `to_uppercase` method from the `unicode_normalization` crate, which supports Unicode characters. 

## See Also

- [String manipulation in Rust](https://www.rust-lang.org/learn/strings)
- [Rust text_io crate](https://crates.io/crates/text_io)
- [Unicode normalization in Rust](https://rust-cli.github.io/book/tutorial/unicode.html)