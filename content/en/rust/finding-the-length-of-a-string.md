---
title:    "Rust recipe: Finding the length of a string"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Why

As programmers, we often come across the need to find the length of a string in our code. Whether it's for validating user input or manipulating data, knowing how to find the length of a string is a crucial skill in any programming language. In this blog post, we'll take a look at how to find the length of a string in Rust, a fast and safe systems programming language.

## How To

To find the length of a string in Rust, we can use the `len()` method, which is available on the `str` type. This method returns the number of bytes in the string, not the number of characters. Let's see an example:

```Rust
let my_string = "Hello, world!";
let length = my_string.len();
println!("The length of the string is: {}", length);
```

This code will output: `The length of the string is: 13`.

But what if we want to find the length of a string in terms of characters? For that, we can use the `chars()` method, which returns an iterator over the characters of the string. We can then use the `count()` method to get the number of elements in the iterator, which will give us the length of the string in characters. Here's an example:

```Rust
let my_string = "안녕하세요"; // "Hello" in Korean
let length = my_string.chars().count();
println!("The length of the string is: {}", length);
```

This code will output: `The length of the string is: 5`.

## Deep Dive

As mentioned earlier, the `len()` method returns the number of bytes in a string, not the number of characters. This is because Rust uses UTF-8 encoding for strings, where some characters take up more than one byte. For example, the Korean character "안" takes up 3 bytes in UTF-8. Therefore, the `len()` method will give us the length of the string in terms of bytes, not characters.

To find the length of a string in terms of characters, we can use the `chars()` method, as shown in the above example. This method works by iterating over the characters of the string, which may take longer to execute compared to the `len()` method. However, it will give us the accurate number of characters in the string.

## See Also

- [Rust docs: str type](https://doc.rust-lang.org/std/primitive.str.html)
- [Rust docs: str.len() method](https://doc.rust-lang.org/std/primitive.str.html#method.len)
- [Rust docs: str.chars() method](https://doc.rust-lang.org/std/primitive.str.html#method.chars)