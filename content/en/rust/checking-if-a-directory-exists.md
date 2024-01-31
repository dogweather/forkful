---
title:                "Checking if a directory exists"
date:                  2024-01-20T14:58:30.687642-07:00
html_title:           "Gleam recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists in Rust is about verifying a folder is present on the file system. Programmers do it to prevent errors when accessing or modifying files, ensuring smoother file operations.

## How to:

Rust's standard library makes this task simple with `std::path::Path` and `std::fs`:

```rust
use std::path::Path;

fn main() {
    let path = Path::new("/some/directory");

    if path.exists() && path.is_dir() {
        println!("Directory exists!");
    } else {
        println!("Directory does not exist.");
    }
}
```

Sample output, if directory exists:
```
Directory exists!
```

Sample output, if directory does not exist:
```
Directory does not exist.
```

## Deep Dive:

Historically, file operations needed more verbose error handling, making the code clunky. Rust simplifies this with concise methods that "just work". Alternatives, like using shell commands or other libraries, exist but aren't as streamlined. The `exists()` method only checks for existence, not for whether it's a directory or a file; combine it with `is_dir()` for directories. These methods internally use the OS's system calls to query the file system efficiently.

## See Also:

- Rust's Path documentation: https://doc.rust-lang.org/std/path/struct.Path.html
- Rust's fs module documentation: https://doc.rust-lang.org/std/fs/
- Error handling in Rust: https://doc.rust-lang.org/book/ch09-00-error-handling.html
