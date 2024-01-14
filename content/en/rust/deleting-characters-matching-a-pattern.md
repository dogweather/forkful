---
title:    "Rust recipe: Deleting characters matching a pattern"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

When working with text data in programming, it's often necessary to clean or manipulate the data. One common task is deleting characters that match a specific pattern. This can be useful for removing unwanted formatting, special characters, or even just simplifying the data. Whether you're a beginner or an experienced programmer, knowing how to delete characters matching a pattern can come in handy in a variety of situations.

## How To

To delete characters matching a pattern in Rust, we can use the `.replace()` method from the standard library's `str` type. This method takes in the pattern to be replaced and the replacement value, which can be an empty string to effectively delete the characters.

Let's take a look at an example:

```Rust
let my_string = "Rust is awesome!";

let modified_string = my_string.replace("e", "");

println!("{}", modified_string);
```

Output:

```Rust
Rust is awsom!
```

In this example, the `.replace()` method replaces all occurrences of the letter "e" with an empty string, effectively deleting them.

We can also use regular expressions to specify more complex patterns to be deleted. For this, we can use the `regex` crate from the Rust Community's crates.io repository. Here's an example of deleting all numbers from a string using regular expressions:

```Rust
use regex::Regex;

let my_string = "I have 3 apples and 5 bananas.";

let numbers_only = Regex::new(r"\d").unwrap();

let modified_string = numbers_only.replace_all(my_string, "");

println!("{}", modified_string);
```

Output:

```Rust
I have apples and bananas.
```

## Deep Dive

Regular expressions open up a world of possibilities for deleting characters matching a pattern in Rust. They allow for more complex patterns to be specified, including special characters, wildcards, and more. In addition to the `regex` crate, there are also other crates available for working with regular expressions in Rust, such as `lazy_static` for compiling regular expressions at compile time for improved performance.

It's worth noting that the `.replace()` method is not limited to just deleting characters. It can also be used to replace them with a different value, making it a powerful tool for text manipulation in Rust.

## See Also

- The Rust Language Reference: <https://doc.rust-lang.org/reference/>
- The Rust Community's crates.io repository: <https://crates.io/>
- The regex crate: <https://crates.io/crates/regex>
- The lazy_static crate: <https://crates.io/crates/lazy_static>