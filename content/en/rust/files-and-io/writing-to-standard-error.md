---
date: 2024-02-03 19:03:38.101347-07:00
description: "Writing to standard error (stderr) in Rust is about directing error\
  \ messages and diagnostics to the console separately from the standard output (stdout).\u2026"
lastmod: '2024-03-13T22:44:59.911028-06:00'
model: gpt-4-0125-preview
summary: "Writing to standard error (stderr) in Rust is about directing error messages\
  \ and diagnostics to the console separately from the standard output (stdout).\u2026"
title: Writing to standard error
weight: 25
---

## What & Why?
Writing to standard error (stderr) in Rust is about directing error messages and diagnostics to the console separately from the standard output (stdout). Programmers do this to differentiate normal program output from error messages, making it easier to handle errors appropriately or redirect them to logs or files during execution.

## How to:
Rust provides a straightforward way to write to stderr using the `eprintln!` macro, similar to how `println!` is used for stdout. Here’s a basic example:

```rust
fn main() {
    eprintln!("This is an error message!");
}
```

Sample output (to standard error):
```
This is an error message!
```

For more control over the error messages, such as when you want to format text or handle I/O results, use the `stderr` function from the `std::io` module. This method provides a handle to the global stderr stream, which you can then write to using methods like `write_all` or `writeln` from the `Write` trait:

```rust
use std::io::{self, Write};

fn main() {
    let stderr = io::stderr();
    let mut handle = stderr.lock();
    
    writeln!(handle, "Formatted error message: {}", 404).expect("Failed to write to stderr");
}
```

Sample output (to standard error):
```
Formatted error message: 404
```

If you are working in environments or applications where you rely on libraries for logging or error handling, libraries such as `log` and `env_logger` are popular. Though they are used more for logging purposes, they are configurable and can direct error log levels to stderr. Below is a simple usage example using `log` and `env_logger`:

First, add the dependencies to your `Cargo.toml`:
```toml
[dependencies]
log = "0.4"
env_logger = "0.9"
```

Then, setup and use the logging in your application:
```rust
fn main() {
    env_logger::init();
    log::error!("This is an error message logged to stderr");
}
```

Running this program (after setting up `env_logger` with an appropriate environment variable, for example, `RUST_LOG=error`) will output the error message to stderr, utilizing the logging infrastructure.

```plaintext
ERROR: This is an error message logged to stderr
```
