---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output means displaying data and code information during or after software execution. Programmers do this to identify errors, assess performance, and understand program flow.

## How to:

The Rust programming language provides `println!` and `dbg!` macros for debug output. Here's an example using `println!`:

```Rust
let mut x = 5;
println!("The value of x is: {}", x);
x = 6;
println!("x is now: {}", x);
```

In this example, we indicate where we want to insert x into the string using `{}`. The values will be displayed as below:

```
The value of x is: 5
x is now: 6
```

To print complex data types (like structs_), add the `#[derive(Debug)]` attribute above struct definition and use `{:?}` to print:

```Rust
#[derive(Debug)]
struct Sample {
    name: String,
    number: i32,
}

fn main() {
    let sample = Sample { name: String::from("Alice"), number: 5 };
    println!("{:?}", sample);
}
```

This prints:

```
Sample { name: "Alice", number: 5 }
```

We can also use `dbg!` macro, which is handy when you want to look at a variable's value and continue with its original logic:

```Rust
fn main() {
    let x = 4;
    let y = dbg!(x * 4) + 1;
    println!("y is {}", y);
}
```

Running this prints:

```
[src/main.rs:4] x * 4 = 16
y is 17
```

## Deep Dive

The history of debug output in languages goes back to the early days of programming. In Rust, `println!` was always preferred for debug output, and users could format output with replacement fields `{}`. However, Rust 1.32 introduced `dbg!`, simplifying debugging for developers.

Alternatives include using a more sophisticated logging framework such as `log` or `env_logger` if you need more flexible logging features such as log levels and file output.

Rust implements debug output as part of the standard library. Underneath, `println!` and `dbg!` expands into calls to `io::stdout().write_fmt`. This I/O operation can block the current thread until it is finished.

## See Also

Rust println! Macro: https://doc.rust-lang.org/std/macro.println.html

Rust dbg! Macro: https://doc.rust-lang.org/std/macro.dbg.html

Rust Logging with env_logger: https://docs.rs/env_logger/0.8.3/env_logger/ 

Rust Debug trait: https://doc.rust-lang.org/std/fmt/trait.Debug.html