---
title:                "Rust recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substrings are snippets of text extracted from a larger string. In Rust, working with substrings can be useful in a variety of situations, such as parsing user input, manipulating text, or implementing search algorithms. In this blog post, we'll take a look at how to extract substrings in Rust and explore some of the underlying concepts.

## How To

To extract a substring in Rust, we can use the `get()` method from the `str` type. This method takes two parameters: the starting position of the substring and the ending position. Both parameters are specified as indices within the original string.

Let's take a look at an example:

```Rust
let my_string = "Hello, world!";
let sub =my_string.get(0..5);
println!("{}", sub); // outputs: `Hello`
```

In this example, we create a string `my_string` and use the `get()` method to extract a substring starting at index 0 and ending at index 5. The resulting substring is then printed to the console.

We can also use the `get()` method with variables to dynamically extract substrings. This is useful when dealing with user input or data from an external source.

```Rust
let my_string = "Hello, world!";
let start = 0;
let end = 5;
let sub = my_string.get(start..end);
println!("{}", sub); // outputs: `Hello`
```

It's also worth noting that the `get()` method returns an `Option` type, which means it can either return a substring or `None` if the specified indices are out of bounds.

## Deep Dive

Under the hood, the `get()` method uses Rust's `Index` trait, which allows us to access elements of a data structure by index. This trait is implemented for the `str` type, which is why we can use the `get()` method on string slices.

When extracting substrings, it's important to understand how indices work in Rust. Rust strings are UTF-8 encoded, which means that not all characters have the same byte length. This can lead to unexpected results when working with indices, so it's important to carefully consider which indices to use when extracting substrings.

We can also use slicing syntax as an alternative to the `get()` method. This syntax allows us to specify ranges of indices directly within square brackets.

```Rust
let my_string = "Hello, world!";
let sub = &my_string[0..5]; // using slicing syntax
println!("{}", sub); // outputs: `Hello`
```

## See Also

- [Rust Documentation: str::get()](https://doc.rust-lang.org/std/primitive.str.html#method.get)
- [Rust Documentation: slicing syntax](https://doc.rust-lang.org/stable/book/ch04-03-slices.html#slicing-strings)
- [Rust Documentation: Index trait](https://doc.rust-lang.org/std/ops/trait.Index.html)
- [Rust Programming Language](https://www.rust-lang.org/)