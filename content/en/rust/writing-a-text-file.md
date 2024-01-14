---
title:                "Rust recipe: Writing a text file"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why 

Writing a text file is an essential task in programming. It allows developers to create and store data in a readable format that can be easily accessed by their programs. Whether you need to save user input, log data, or export results, learning how to write a text file in Rust will greatly enhance your programming skills.

## How To

First, we will need to import the `std::fs` module, which provides functions for working with files. We will also need the `std::io::Write` trait to handle writing operations. Let's start by creating a file named "output.txt" and opening it for writing:

```Rust
use std::fs::File;
use std::io::Write;

fn main() {
    let mut file = File::create("output.txt").expect("Unable to create file");
}
```

Next, we can use the `write!` macro to write data to our file. This macro takes in the file handle and the data to be written. Let's write the string "Hello World!" to our file:

```Rust
write!(file, "Hello World!").expect("Unable to write data to file");
```

Finally, we need to flush the changes to the file and close it using the `flush()` and `close()` functions respectively:

```Rust
file.flush().expect("Unable to flush changes to file");
```

```Rust
file.close().expect("Unable to close file");
```

Congratulations, you have just written your first text file in Rust!

## Deep Dive

In the previous section, we used the `expect()` function to handle potential errors while writing to the file. This function expects a value of type `Result` and will panic if the value is an `Err` enum. 

Additionally, we used the `mut` keyword to make the `file` variable mutable. This allows us to make changes to the file after its creation. We also used the `write!` macro instead of the `write()` function since it provides better error handling and formatting capabilities.

It is also worth noting that in Rust, files are opened in "write-only" mode by default. This means that you cannot read from the file unless you explicitly open it in read mode. This is done for safety reasons, as it prevents unintentional overwriting of data.

## See Also

For more information on file management in Rust, check out the official Rust documentation on the `std::fs` module [here](https://doc.rust-lang.org/std/fs/).

You can also learn more about Rust syntax and coding conventions by reading the Rust Book [here](https://doc.rust-lang.org/book/).

Happy coding!