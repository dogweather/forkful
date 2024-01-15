---
title:                "Concatenating strings"
html_title:           "Rust recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings is a common task in programming, especially when dealing with user input or generating dynamic output. By combining multiple strings into one, you can easily manipulate and display information in a readable format.

## How To

To concatenate strings in Rust, we can use the `+` operator or the `format!` macro. Let's take a look at some examples using both methods:

```
// Using the `+` operator
let greeting = "Hello";
let name = "John";
let message = greeting + ", " + name + "!"; // Output: Hello, John!
```

```
// Using the `format!` macro
let greeting = "Hello";
let name = "John";
let message = format!("{}, {}!", greeting, name); // Output: Hello, John!
```

As you can see, both methods result in the same output. However, the `format!` macro allows for more flexibility as it can handle any data type, not just strings.

## Deep Dive

Now, let's dive deeper into concatenating strings in Rust. When using the `+` operator, the compiler will implicitly convert the strings into `String` objects. This means that you don't need to explicitly create a `String` object before concatenating.

Additionally, you can also use the `String::push_str()` method to append a string to an existing `String` object. This can be useful when building a string in multiple steps.

```
let mut message = String::from("Hello, ");
message.push_str("John!"); // Output: Hello, John!
```

Another important aspect to note is that concatenating strings can be inefficient, especially if you're dealing with large strings. This is because a new `String` object is created every time you use the `+` operator or `format!` macro. To avoid this, it is recommended to use the `String::push()` or `String::insert()` methods to modify an existing `String` object.

## See Also

For more information on string manipulation in Rust, check out the official documentation and other useful resources:

- [Rust Strings](https://doc.rust-lang.org/std/string/)
- [Rust Book - Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Learn Rust in Y Minutes - Strings](https://learnxinyminutes.com/docs/rust/#strings)