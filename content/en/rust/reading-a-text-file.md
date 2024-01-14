---
title:                "Rust recipe: Reading a text file"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Reading a text file may seem like a simple task, but it is an essential skill for any programmer. Whether you need to process large amounts of data or simply extract information from a file, knowing how to read a text file efficiently can save you time and frustration. In this blog post, we will explore how to read a text file using Rust programming language.

## How To

To read a text file in Rust, we first need to open the file and create a `File` object. We can do this using the `std::fs::File` module, which provides methods for file manipulation. We also need to import the `io::prelude` module to read and write to the file. Here is an example code that reads a text file named "data.txt" and prints its contents to the console:

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::open("data.txt").expect("Failed to open file"); // open the file
    let mut contents = String::new(); // create an empty string
    file.read_to_string(&mut contents).expect("Failed to read file"); // read file and store contents in string
    println!("{}", contents); // print contents to console
}
```

Running this code will output the contents of the "data.txt" file. We can also use the `std::io::BufReader` module for more efficient reading of large text files. Here is an example code using `BufReader`:

```Rust
use std::fs::File;
use std::io::{self, BufReader};
use std::io::prelude::*;

fn main() -> io::Result<()> {
    let file = File::open("data.txt")?; // open the file
    let buf_reader = BufReader::new(file); // create a buffer reader object
    for line in buf_reader.lines() {
        println!("{}", line?); // print each line to console
    }
    Ok(())
}
```

This code will print out each line in the "data.txt" file.

## Deep Dive

Behind the scenes, Rust uses a `Read` trait to read from a file. This trait allows different types of objects, such as `File` and `BufReader`, to implement read operations in a similar manner. The `Read` trait provides methods such as `read`, which reads bytes from a file into a buffer, and `read_to_string`, which reads bytes and converts them into a `String`.

Rust also handles errors gracefully using the `?` operator, which is used to propagate errors instead of using `expect` to handle them. This helps in writing more robust and reliable code.

## See Also

- [Rust Standard Library documentation for File and BufReader](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Rust Book - The Read trait](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html#the-read-trait)
- [Rust By Example - File I/O](https://rustbyexample.com/std_misc/file.html)