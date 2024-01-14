---
title:                "Rust recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of programming, and one of the most common methods for debugging is printing out debug output. This can help you understand and track the flow of your code, as well as identify any potential errors or bugs. In Rust, printing debug output is particularly useful due to its strong type system and error handling, making it a valuable tool for any Rust programmer.

## How To

Printing debug output in Rust is simple and straightforward. All you need to do is use the `println!()` macro, which allows you to print out a string along with any variables or values you want to see. Here's an example:

```Rust
let name = "John";
let age = 25;
println!("My name is {} and I am {} years old.", name, age);
```

This will output: `My name is John and I am 25 years old.`

You can also use the `format!()` macro to store the output in a string and use it later. This can be useful when debugging specific functions or methods. Here's an example:

```Rust
let name = "Jane";
let address = "123 Main St.";
let details = format!("Name: {}, Address: {}", name, address);
println!("{}", details);
```

This will output: `Name: Jane, Address: 123 Main St.`

## Deep Dive

While printing debug output is simple in Rust, there are some additional features that can make it even more powerful. One of these features is the ability to define your own formatting for types by implementing the `Debug` trait for structs or enums. This allows you to customize how these types are printed when using `println!()` or `format!()`.

Another useful feature is the `dbg!()` macro, which prints out the value of an expression along with its source location. This can be particularly helpful when debugging complex code or identifying the source of an error.

You can also use `eprintln!()` and `eprint!()` macros to print debug output to `stderr` instead of `stdout`. This can be helpful when you want to differentiate between regular output and debug output, or when stdout is being redirected.

## See Also

- Official Rust documentation on printing: https://doc.rust-lang.org/std/macro.println.html
- Blog post on debugging techniques in Rust: https://blog.logrocket.com/debugging-rust-programs/
- Video tutorial on printing debug output in Rust: https://www.youtube.com/watch?v=KlU-FnhD0ME