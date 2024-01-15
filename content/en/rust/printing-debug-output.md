---
title:                "Printing debug output"
html_title:           "Rust recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of the coding process, and one of the most commonly used techniques is printing debug output. It allows programmers to see the values of variables, function outputs, and other important information at a specific point in their program's execution. This can help identify bugs and errors more quickly, making the debugging process more efficient.

## How To

There are a few different ways to print debug output in Rust, depending on what you want to see and where you want to see it. Here are some examples using the `println!` macro:

```Rust
// Print a string
let name = "John";
println!("Hello, {}!", name);
```

```Rust
// Print an integer
let num = 42;
println!("The answer to everything is {}.", num);
```

```Rust
// Print a boolean
let is_rust_fun = true;
println!("Is Rust fun? {}", is_rust_fun);
```

You can also use the `dbg!` macro to print the value of an expression along with its location in the code:

```Rust
// Print the length of a vector
let num_list = vec![1, 2, 3];
dbg!(num_list.len());
```

The `eprintln!` and `eprint!` macros can be used to print debug output to the standard error stream, which can be useful for logging errors and messages during the debugging process.

## Deep Dive

Rust also offers the `format!` macro, which works similarly to `println!` but returns a `String` instead of printing directly to the console. This can be useful if you want to save the debug output for later use.

Additionally, Rust has a powerful logging crate called `log` that allows for more advanced debugging and logging capabilities. It provides macros for different logging levels, allowing you to customize the output based on the severity of the message.

Overall, printing debug output in Rust is an important tool for debugging and error tracking, and there are multiple ways to achieve it, depending on your specific needs.

## See Also

- [Rust Book: Handling Errors](https://doc.rust-lang.org/book/ch09-00-error-handling.html)
- [Rust Reference: Debugging in Rust](https://doc.rust-lang.org/edition-guide/rusti/debugging.html)
- [Rust Pub: log crate](https://crates.io/crates/log)