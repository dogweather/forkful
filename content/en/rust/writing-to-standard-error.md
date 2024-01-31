---
title:                "Writing to standard error"
date:                  2024-01-19
simple_title:         "Writing to standard error"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error (stderr) is outputting text to the error stream, separate from standard output (stdout). Programmers do it for logging errors and diagnostic messages without cluttering the regular output that might be redirected or piped into other programs.

## How to:

Rust makes writing to stderr simple. Use the `eprintln!` macro for text, just like `println!` but for errors.

```Rust
fn main() {
    // Regular output
    println!("This is a regular message.");

    // Error output
    eprintln!("This is an error message.");
}
```

Sample output:

```shell
This is a regular message.
This is an error message.
```

Notice the error message goes to stderr. In a terminal, you won't see the difference. However, if you redirect stdout, stderr still shows up in the console.

```shell
$ cargo run > output.txt
This is an error message.
```

Here `output.txt` will only contain "This is a regular message."

## Deep Dive

Historically, separating stdout and stderr allows Unix systems to handle regular and error data differently. It's good practice and helps with automation and logging.

Alternatives for writing to stderr are lower-level, like using `std::io::stderr`. It gives more control and works well for non-text data.

```Rust
use std::io::{self, Write};

fn main() -> io::Result<()> {
    let stderr = &mut io::stderr();
    
    // Write a string directly to stderr
    writeln!(stderr, "Error: Could not complete the operation")?;
    
    Ok(())
}
```

Under the hood, `eprintln!` is a macro wrapping `writeln!` to stderr, keeping things DRY (Don't Repeat Yourself).

## See Also

For more on error handling and logging:

- Rust By Example on stdio: https://doc.rust-lang.org/rust-by-example/std_misc/stdio.html
- The Rust Book on Error Handling: https://doc.rust-lang.org/book/ch09-00-error-handling.html
- The Rust `log` crate for a more comprehensive logging setup: https://crates.io/crates/log
