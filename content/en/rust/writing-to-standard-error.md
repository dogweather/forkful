---
title:                "Writing to standard error"
html_title:           "Rust recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to standard error is a crucial aspect of programming in Rust, as it allows for debugging and error handling in a more efficient and organized manner. It also helps to provide better user experience by displaying relevant error messages.

## How To

To write to standard error in Rust, we will be using the `eprintln!` macro. This macro works similar to the `println!` macro, except it prints to the standard error stream instead of the standard output stream. Let's take a look at an example:

```Rust
fn main() {
    let name = "John";
    eprintln!("Hello, {}!", name);
}
```

In the above code, we are using the `eprintln!` macro to print a greeting to the standard error stream. When we run this code, the output will be:

```
Hello, John!
```

Now, let's see how we can use this macro for error handling. Imagine we have a function that performs division and returns a result. We want to print an error if the divisor is zero. We can use the `eprintln!` macro to achieve this:

```Rust
fn divide(x: i32, y: i32) -> i32 {
    if y == 0 {
        eprintln!("Error: Divisor cannot be zero!");
        return -1;
    }
    x / y
}

fn main() {
    let result = divide(10, 0);
}
```

In the above code, we check for the edge case where the divisor is zero and use the `eprintln!` macro to display an error message. This helps us to identify and handle any potential errors in our code.

## Deep Dive

In Rust, standard error is represented by the `std::io::stderr` struct. The `eprintln!` macro uses this struct as the destination for printing the message. This means we can also explicitly use the `std::io::Write` trait to write to the standard error stream. Let's take a look at an example:

```Rust
use std::io::{self, Write};

fn main() {
    let mut stderr = io::stderr();
    stderr.write(b"Hello from standard error!\n").unwrap();
}
```

In the above code, we are using the `write` method from the `std::io::Write` trait to explicitly write to the standard error stream. We use the `unwrap` method to handle any potential errors that may occur.

## See Also

- [Rust Language Documentation](https://www.rust-lang.org/learn)
- [Debugging in Rust](https://docs.rs/debugging/)
- [Rust Error Handling](https://doc.rust-lang.org/error-index.html)