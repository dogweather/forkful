---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file allows us to get contents from a file into our program for processing. We do this to extract, manipulate or output data.

## How to:

Reading a text file in Rust is straightforward thanks to its standard library. Here's a simple function that opens a file and reads its contents:

```Rust
use std::io::Read;
use std::fs::File;

fn main() {
    let mut file = File::open("file.txt").expect("Could not open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Failed to read file");
    println!("{}", contents);
}
```

When run, our Rust program will read and print the contents of `file.txt`. If it fails to either open or read the file, an error message will be printed.

## Deep Dive

While Rust's approach to file reading is heavily influenced by C and C++, it diverges by adding better error handling and type safety. There are several methods to read a file. We used `read_to_string()`, which reads into a String for convenience. Alternatively, you can read into a byte buffer using `read()` if you need more control.

Under the hood, `File::open` opens the file, while `read_to_string` allocates a new String, then reads the file's contents into it. In case of failures, `expect()` is called and if the result is not `Ok`, the program will panic and print the provided error message.

## See Also

For more on the topic, check out:

1. [The Rust Programming Language][1], specifically the [Reading a File][2] section.
2. [Rust By Example][3]: A collection of runnable examples that illustrate various Rust concepts and standard libraries.
3. [Rust IO documentation][4]: Details about everything you can do with Rust's IO functionality.

[1]: https://doc.rust-lang.org/book/
[2]: https://doc.rust-lang.org/book/ch05-02-strings.html#storing-utf-8-encoded-text-with-strings
[3]: https://doc.rust-lang.org/stable/rust-by-example/
[4]: https://doc.rust-lang.org/std/io/index.html