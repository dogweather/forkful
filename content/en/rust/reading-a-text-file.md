---
title:                "Reading a text file"
html_title:           "Rust recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Reading a text file in Rust refers to the process of accessing and retrieving data from a file that contains text-based information. Programmers often do this to utilize external data or configurations in their programs, without having to hardcode them into the source code.

## How to:
Reading a text file in Rust is a relatively simple process, thanks to the standard library's `std::fs` module. Here's a quick example:

```Rust
use std::fs;
# Read the file contents as a string
let contents = fs::read_to_string("example.txt").expect("Could not read file.");

# Read the file contents into a vector of bytes
let contents = fs::read("example.txt").expect("Could not read file.");

# Read the file line by line
let file = fs::File::open("example.txt").expect("Could not open file.");
let reader = io::BufReader::new(file);
for line in reader.lines() {
    println!("{}", line.expect("Could not read line."));
}
```

The first two examples use the `fs::read_to_string` and `fs::read` functions respectively, to read the entire contents of the file into a string or vector of bytes. The `expect` method is used to handle any potential errors.

The third example utilizes the `fs::File` struct, which represents a file and provides methods for reading, writing, and manipulating it. By combining it with the `io::BufReader` struct, we can read the file line by line using the `lines` method.

## Deep Dive:
The concept of reading a text file has been around since the early days of computing, and has since become a fundamental aspect of programming. In Rust, there are alternative methods for reading a file, such as using the `std::io::Read` trait or the `std::fs::metadata` function.

When reading a text file, it is important to understand the encoding used in the file to correctly interpret the data. Rust's standard library provides various methods for handling different encodings, such as `fs::read_to_string` which uses UTF-8 by default.

## See Also:
- [fs::read_to_string documentation](https://doc.rust-lang.org/std/fs/fn.read_to_string.html)
- [fs::read documentation](https://doc.rust-lang.org/std/fs/fn.read.html)
- [std::fs module documentation](https://doc.rust-lang.org/std/fs/index.html)
- [std::io module documentation](https://doc.rust-lang.org/std/io/index.html)