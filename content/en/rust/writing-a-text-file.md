---
title:                "Writing a text file"
html_title:           "Rust recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Creating and writing to a text file is a common task in programming that involves storing data in a designated file on a computer. Programmers use text files to store and access information that may need to be retrieved or updated at a later time.

## How to:

Writing to a text file in Rust is straightforward and can be accomplished using the standard library's `std::fs` module. The following code shows an example of how to create a new text file called "data.txt" and write the text "Hello world!" to it:

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // Open the file in write-only mode
    let mut file = File::create("data.txt")
        .expect("Failed to create file");

    // Write the text to the file
    file.write_all(b"Hello world!")
        .expect("Failed to write to file");
}
```

The `File::create()` method creates a new file and returns a `File` object. The `write_all()` method then writes the specified bytes to the file, which, in this case, is the string "Hello world!" converted into bytes using the `b` prefix. The resulting text file will contain the phrase "Hello world!".

## Deep Dive:

Writing to a text file has been around since the early days of programming and is still widely used today. Alternatives to writing to a text file include using a database or an in-memory store, but text files remain a simple and effective way to store and access data.

In Rust, working with files can be accomplished using the standard library or third-party libraries like `fs_extra` or `path_abs`. Under the hood, writing to a text file involves opening a file, handling errors, converting data into bytes, and writing those bytes to the file.

## See Also:

- [Writing files in Rust](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Rust standard library documentation](https://doc.rust-lang.org/std/)