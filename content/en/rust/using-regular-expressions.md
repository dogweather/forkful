---
title:    "Rust recipe: Using regular expressions"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions may seem intimidating at first, but they are an invaluable tool for any programmer looking to manipulate or search through text. Whether it's extracting data from a text file, validating user input, or finding patterns in a large dataset, regular expressions can greatly simplify and streamline these tasks.

## How To

To use regular expressions in Rust, you first need to import the `regex` crate by adding it to your `Cargo.toml` file. Then, you can use the `Regex` struct from the crate to create a regular expression object. Let's take a look at a simple example:

```Rust
use regex::Regex;

let re = Regex::new(r"Hello, (.*?)!").unwrap();
let text = "Hello, world!";
let result = re.captures(text).unwrap();

println!("Greetings from {}!", result.get(1).unwrap().as_str()); // Greetings from world!
```

In this example, we use the regular expression `Hello, (.*?)!` which will match any text between the words "Hello," and "!". By using the `captures` method, we can extract the captured text and use it in our output.

Regular expressions also support more complex patterns and special characters, making it a powerful tool for text manipulation. Below is a more advanced example:

```Rust
let re = Regex::new(r"(\d{3})-(\d{3})-(\d{4})").unwrap();
let text = "My phone number is 555-123-4567.";
let result = re.replace_all(text, "($1) $2-$3");

println!("{}", result); // My phone number is (555) 123-4567.
```

In this example, we use the `\d` special character to match any digit and the `{}` syntax to specify the exact number of digits we want to match. By using capture groups, we can then rearrange the order of the digits in the output text.

## Deep Dive

Regular expressions can also be used for more advanced tasks, such as finding and replacing text in multiple files at once or validating complex data formats. However, it's important to note that regular expressions are not always the most efficient solution for these tasks and using them for large datasets or complex patterns may result in significant performance issues.

One important thing to keep in mind when using regular expressions is that it's easy to create overly complicated patterns that become difficult to read and maintain. It's best to start with simpler patterns and gradually add complexity as needed.

To learn more about the syntax and advanced features of regular expressions in Rust, check out the official [Rust documentation](https://doc.rust-lang.org/std/regex/). You can also experiment with different patterns using [regex101](https://regex101.com/) or [Rust Playground](https://play.rust-lang.org/).

## See Also

- [Rust documentation on regex](https://doc.rust-lang.org/std/regex/)
- [regex101](https://regex101.com/): online regex editor and tester
- [Rust Playground](https://play.rust-lang.org/): online Rust compiler and playground