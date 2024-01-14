---
title:                "Rust recipe: Concatenating strings"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

String concatenation is a common operation in programming that involves combining two or more strings into one. This can be useful for tasks such as creating dynamic messages, formatting data, or building URLs.

## How To

In Rust, there are several ways to concatenate strings. Let's take a look at three approaches using the built-in string type, `String`.

### Plus Operator

The most straightforward way to concatenate strings in Rust is by using the `+` operator. This works by taking ownership of the first string and appending the second string to it. Here's an example:

```Rust
let hello = String::from("Hello, ");
let name = String::from("world!");

let message = hello + &name;
// 'hello' is moved into 'message' and can no longer be used

println!("{}", message); // Output: Hello, world!
```

### `format!` Macro

Another approach is to use the `format!` macro, which constructs a new `String` by combining formatted strings or values. This does not take ownership of the original strings, making it useful for situations where you want to reuse them. Here's an example:

```Rust
let name = String::from("John");
let age = 25;

let info = format!("{} is {} years old.", name, age);

println!("{}", info); // Output: John is 25 years old.
```

### `String::push_str` Method

Lastly, you can use the `push_str` method to append a string to an existing `String` without taking ownership. This is useful when you want to avoid copying or allocating new memory. Here's an example:

```Rust
let mut message = String::from("Hello, ");

message.push_str("world!");
// 'message' can continue to be modified afterwards

println!("{}", message); // Output: Hello, world!
```

## Deep Dive

Concatenating strings in Rust may seem simple, but there are some important details to consider. One key thing to note is that the `+` operator creates a new `String` and copies both strings into it, which can be inefficient for large strings. The `format!` macro and `push_str` method are more efficient because they do not involve copying or allocating new memory.

It's also worth mentioning that Rust strings are UTF-8 encoded and therefore support a variety of characters, including non-Latin alphabets and emoji. This means that you can concatenate strings containing different characters without any issues.

## See Also

- [Rust String](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust String concatenation](https://doc.rust-lang.org/rust-by-example/std/str/concatenate.html)
- [Unicode in Rust](https://doc.rust-lang.org/book/ch08-02-strings.html#unicode-graphemes-and-grapheme-clusters-can-be-problematic-for-latin-letters-too)