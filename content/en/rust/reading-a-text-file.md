---
title:                "Rust recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Text files are a common form of data storage and retrieval, and being able to read and parse them is a necessary skill for any programmer. If you're learning Rust, understanding how to read and manipulate text files is a great way to practice using the language's features.

## How To
To read a text file in Rust, we first need to open it using the `File::open()` method. This function returns a `Result` type, which we can use to handle any potential errors that may occur. Let's see an example of reading a file named "data.txt" and printing its contents:

```rust
use std::fs::File;
use std::io::{BufReader, BufRead};

fn main() {
    if let Ok(file) = File::open("data.txt") {
        let reader = BufReader::new(file);
        for line in reader.lines() {
            println!("{}", line.unwrap());
        }
    } else {
        println!("Error opening file.");
    }
}
```

In this code, we use the `BufReader` type to efficiently read the contents of the file line by line. The `lines()` method returns an iterator, which we can use to loop through all the lines in the file. For each line, we use `unwrap()` to get the actual string value. This is just a basic example, but there are many ways to read and manipulate text files in Rust.

## Deep Dive
Now, let's take a closer look at how the `BufReader` type works. When we call the `lines()` method, it actually returns an iterator of `io::Result<String>`. This means each line can potentially contain an error, which we handle using the `unwrap()` method. However, this is not an ideal way to handle errors, as it will panic if an error occurs. To handle errors properly, we can use the `match` expression or `expect()` method.

Additionally, we can also specify the character encoding when reading a text file. By default, UTF-8 encoding is assumed, but we can use the `encoding_from_uitf8()` function to detect the encoding of the file and read it accordingly.

## See Also 
- [Rust Documentation: File](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Rust Documentation: BufReader](https://doc.rust-lang.org/std/io/struct.BufReader.html)
- [Rust Documentation: Result](https://doc.rust-lang.org/std/result/)
- [Rust Cookbook: Reading a File Line by Line](https://rust-lang-nursery.github.io/rust-cookbook/file/read-write.html#read-a-file-line-by-line)