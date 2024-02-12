---
title:                "Printing debug output"
aliases:
- /en/rust/printing-debug-output/
date:                  2024-01-20T17:53:24.504452-07:00
model:                 gpt-4-1106-preview
simple_title:         "Printing debug output"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Printing debug output lets you peek at your program's state without a full-blown debugger. It's quick, dirty, and perfect for tracing pesky bugs when you don't need the firepower of a dedicated debugging tool.

## How to:

To print something simple, use `println!`. If you need to print a value for debugging, `dbg!` comes in handy.

```Rust
fn main() {
    let mut vec = vec![1, 2, 3];
    
    // Basic printing
    println!("Hello, Rustaceans!");

    // Debug formatting with println! using `{:?}`
    println!("{:?}", vec);

    // Debug with `dbg!`, prints to stderr and returns the value
    dbg!(&vec);

    // Modifying vec after using `dbg!`
    vec.push(4);
    dbg!(vec);
}
```

Sample output:

```
Hello, Rustaceans!
[1, 2, 3]
[src/main.rs:9] &vec = [
    1,
    2,
    3,
]
[src/main.rs:13] vec = [
    1,
    2,
    3,
    4,
]
```

## Deep Dive

Printing debug output has been a straightforward part of programming since the early days. Its simplicity often makes it a go-to choice for quickly diagnosing problems.

In Rust, `println!` is great for displaying user-friendly messages. The magic comes with `dbg!`, introduced in Rust 1.32, which prints both the value and its location in the code. It outputs to standard error (stderr), so it won't mix with standard output (stdout) and can be separately redirected if needed. 

For complex types, you can derive the `Debug` trait to automatically create a format that `println!` and `dbg!` can use. That's what the `#[derive(Debug)]` annotation does above your structs and enums.

As for alternatives, proper loggers exist such as `log` and `env_logger`, and if you need more granular control, consider a debugger such as `gdb` or `lldb`, which work with Rust through integrations like `rust-gdb` or `rust-lldb`.

## See Also

For more on Rust's debug printing and formatting options:

- The Rust Book on `println!` and Formatting: https://doc.rust-lang.org/std/fmt/index.html
- The `dbg!` macro documentation: https://doc.rust-lang.org/std/macro.dbg.html
- Official guide for debugging with `gdb` and `lldb`: https://rust-lang.github.io/rustup-components-history
- `log` crate for a more structured approach to logging: https://crates.io/crates/log
- `env_logger` crate, a common logger implementation for the `log` facade: https://crates.io/crates/env_logger
