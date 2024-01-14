---
title:                "Rust recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Rust is a relatively new programming language that has gained popularity for its focus on performance, safety, and concurrency. One common task in programming is finding the length of a string, which gives us important information about the data we are working with. In this blog post, we'll explore how this can be done efficiently in Rust and why it's an important skill to have in your programming toolbox.

## How To

The first step in finding the length of a string is to declare a string variable using the `let` keyword. Next, we can use the `.len()` method to get the length of the string. Let's see how this is done in Rust:

```Rust
let my_string = String::from("Hello, world!");
println!("The length of the string is {}", my_string.len());
```

Running this code will give us the output:

```
The length of the string is 13
```

We can also use the `.len()` method on string literals:

```Rust
let my_literal = "Hello, world!";
println!("The length of the string is {}", my_literal.len());
```

This will give us the same output as before.

We can also use a for loop and the `chars()` method to iterate through each character in the string and manually count the number of characters. However, this method is less efficient and should only be used if necessary.

```Rust
let my_string = String::from("Hello, world!");
let mut count = 0;
for character in my_string.chars() {
    count += 1;
}
println!("The length of the string is {}", count);
```

## Deep Dive

Rust's `String` type is a wrapper over a sequence of bytes, making it a flexible and efficient way to store and manipulate text data. The `String` type also comes with a variety of useful methods, including the `.len()` method which returns the number of bytes in the string. This value can be different from the number of characters, as some characters may be represented by more than one byte.

To handle this potential discrepancy, Rust also has the `.chars()` method which returns an iterator over the Unicode characters of the string. This allows us to accurately count the number of characters in a string, regardless of the number of bytes used to represent each character.

It's also worth mentioning that Rust's `str` type, which is used for string literals, has the same methods as the `String` type, making it consistent and easy to use when dealing with strings.

## See Also

- Rust Documentation: [Strings and Characters](https://doc.rust-lang.org/book/ch08-02-strings.html)
- Rust by Example: [String Length](https://doc.rust-lang.org/rust-by-example/std/str.html#length)