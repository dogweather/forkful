---
title:                "Checking if a directory exists"
html_title:           "Rust recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Checking if a directory exists is an important task in programming, especially when dealing with file manipulation or user input. Knowing how to accurately check a directory's existence can help prevent errors and improve the overall functionality of your code.

## How To

To check if a directory exists in Rust, we can use the `std::fs::metadata` function which returns a `Result` containing information about the directory. Here's an example code block:

```rust
use std::fs;

let dir_path = "my_directory";

if let Ok(metadata) = fs::metadata(dir_path) {
    if metadata.is_dir() {
        println!("{} exists!", dir_path);
    } else {
        println!("{} is not a directory.", dir_path);
    }
} else {
    println!("{} does not exist.", dir_path);
}
```

In this code, we first declare the directory path we want to check. Then using the `fs::metadata` function, we retrieve the metadata for that directory. If the directory exists, we can use the `is_dir()` method to check if it is indeed a directory. If it is, we print a message indicating that it exists. Otherwise, we print a message stating that it is not a directory. If the `fs::metadata` function returns an error, we can assume that the directory does not exist.

## Deep Dive

Behind the scenes, the `fs::metadata` function makes use of the underlying operating system's file system APIs to retrieve information about the directory. This means that the behavior of checking for a directory's existence may differ depending on the operating system being used. For example, in Windows, the function may consider reparse points in the path and follow them to determine if the directory exists.

Additionally, it is important to note that simply checking for a directory's existence does not guarantee that it will not be modified or deleted by another process in the time between the check and the actual use of the directory.

## See Also

- [std::fs::metadata documentation](https://doc.rust-lang.org/std/fs/fn.metadata.html)
- [std::fs::File documentation](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Checking existence of a file or directory in Rust](https://www.mattgathu.com/ruby-tricks/checking-file-existence-rust/)