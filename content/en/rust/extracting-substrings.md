---
title:    "Rust recipe: Extracting substrings"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Why
Rust is a popular systems programming language known for its strong performance and memory safety guarantees. One of the many useful features it offers is the ability to easily extract substrings from strings. In this blog post, we will explore how to use this feature and why it can be beneficial in our code.

## How To
To extract a substring in Rust, we can use the `get()` method from the `str` type. This method takes two arguments - the starting index and the ending index of the substring. Let's take a look at an example:

```Rust
let original_string = "Rust programming is awesome!";
let substring = original_string.get(5..15);
println!("{}", substring); // Output: programming
```

In this example, we used the `get()` method to extract the substring "programming" from the original string. The indices are inclusive on the starting index but exclusive on the ending index, meaning the character at the ending index is not included in the substring. Additionally, the indices are 0-based, so the first character in the string has an index of 0.

We can also use the `get()` method to extract characters at a specific index without specifying an ending index. Let's see how:

```Rust
let name = "John";
let first_letter = name.get(0..1);
println!("{}", first_letter); // Output: J
```

Another way to extract substrings in Rust is by using the `split_at()` method. This method takes an index as an argument and returns two substrings - one from the beginning of the string to the specified index, and another from the index to the end of the string. Here's an example:

```Rust
let sentence = "I love Rust programming";
let (first_part, second_part) = sentence.split_at(7);
println!("{} and {}", first_part, second_part); // Output: I love and Rust programming
```

## Deep Dive
Rust's ability to extract substrings is made possible by its ownership and borrowing system. The `get()` and `split_at()` methods return a `&str` type, which is a borrowed reference to the original string. This means that the extracted substring is still referencing the original string, so no data is unnecessarily copied in memory.

It's also worth noting that the `get()` method returns an `Option<&str>`, which means it could potentially return `None` if the specified indices are out of bounds. This allows us to handle any potential errors gracefully in our code.

## See Also
- The Rust Programming Language book: https://doc.rust-lang.org/book/
- Official Rust website: https://www.rust-lang.org/
- Rust subreddit: https://www.reddit.com/r/rust/