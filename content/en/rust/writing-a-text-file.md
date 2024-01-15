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

## Why
Text files are the backbone of programming. They are used for a variety of purposes, from storing data to creating configuration files. Learning how to write a text file in Rust can significantly enhance your coding skills and allow you to manipulate data in a more efficient and structured way.

## How To
Writing a text file in Rust is a straightforward process and can be done in a few simple steps:

1. First, we need to create a [`File`](https://doc.rust-lang.org/std/fs/struct.File.html) object to work with. This can be done using the [`fs::File::create()`](https://doc.rust-lang.org/std/fs/fn.File.html#method.create) function, which takes in a [`Path`](https://doc.rust-lang.org/std/path/struct.Path.html) object as an argument. The `Path` can be either a relative or absolute path to the file we want to create.
2. Next, we can use the [`write_all()`](https://doc.rust-lang.org/std/io/trait.Write.html#method.write_all) method on the `File` object to write data to the file. This method takes in a `&[u8]` (byte slice) as its argument, so we need to convert our data to a byte slice before passing it in.
3. Finally, we need to handle any potential errors that may occur while writing to the file. This can be done using [`Result`](https://doc.rust-lang.org/std/result/) and [`expect()`](https://doc.rust-lang.org/std/result/enum.Result.html#method.expect) to print out an error message if something goes wrong.

Below is an example of how we can write a text file in Rust:

```rust
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
    // Create a `File` object
    let file = File::create(Path::new("example.txt"));
    
    // Convert our data to a byte slice
    let data = "Hello, world!".as_bytes();
    
    // Write data to the file
    file.unwrap().write_all(data).expect("Unable to write file!");
}
```

Running this code will create a new file named "example.txt" in the same directory as our Rust file and write the text "Hello, world!" to it.

## Deep Dive
The [`File`](https://doc.rust-lang.org/std/fs/struct.File.html) object has various other methods that can be used for writing to a text file, such as [`write()`](https://doc.rust-lang.org/std/io/trait.Write.html#tymethod.write), [`write_fmt()`](https://doc.rust-lang.org/std/io/trait.Write.html#tymethod.write_fmt), [`flush()`](https://doc.rust-lang.org/std/io/trait.Write.html#tymethod.flush), and more. These provide more control over the writing process, such as writing formatted data or flushing the data to the file before it is complete.

Additionally, Rust also has the [`std::io::BufWriter`](https://doc.rust-lang.org/std/io/struct.BufWriter.html) struct, which can be used to improve performance when writing large amounts of data to a file.

## See Also
- [Rust Documentation: std::fs](https://doc.rust-lang.org/std/fs/index.html)
- [Rust Documentation: std::io](https://doc.rust-lang.org/std/io/index.html)
- [Rust By Example: File I/O](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)