---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists involves verifying the presence of a particular directory or folder in a file system. It's useful to ensure we don't repeat operations like creation or deletion if a directory already exists or prevent errors when accessing non-existent directories.

## How to:

In Rust, the `std::path::Path` and `std::fs` modules provide methods for checking if a directory exists. The `exists()` method is particularly useful in this case:

```Rust
use std::path::Path;

fn main() {
    let dir = Path::new("/some/directory/path");

    if dir.exists() {
        println!("Directory exists.");
    } else {
        println!("Directory does not exist.");
    }
}
```

When you run this code, if the directory "/some/directory/path" exists, you'll see "Directory exists.", otherwise, you'll see "Directory does not exist.".

## Deep Dive

Historically, programmers have had different tactics to check for the existence of directories across programming languages. In Unix systems, the command-line and shell scripts provided tools to do this.

Rust, being a language focused on system-level operations, provides this functionality through the standard library in `std::path::Path`. Importantly, using the `exists()` method isn't always the best solution: if you want to ensure a directory also has the correct permissions or verify it's indeed a directory (not a file), prefer `metadata()` and the `is_dir()` methods.

```Rust
use std::path::Path;

fn main() {
    let dir = Path::new("/some/directory/path");

    match dir.metadata() {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("Directory exists.");
            } else {
                println!("Not a directory.");
            }
        },
        Err(_) => println!("Directory does not exist."),
    }
}
```

This code provides more granular control over error handling and verifies that the path in question is indeed a directory.

## See Also

To learn more about interacting with the filesystem in Rust, check out the following resources:

- Official Rust Documentation: [std::fs](https://doc.rust-lang.org/std/fs/index.html), [std::path](https://doc.rust-lang.org/std/path/index.html)
- [Rust by Example – Filesystem Operations](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)