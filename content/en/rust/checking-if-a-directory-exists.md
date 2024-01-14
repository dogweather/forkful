---
title:                "Rust recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why
As a Rust programmer, there may be instances where you need to check if a directory exists before performing certain actions, such as creating new files or navigating within your file system. This is important for ensuring the stability and functionality of your program.

## How To
To check if a directory exists in Rust, we will be using the `std::fs` module which provides basic file and directory manipulation functionalities. The specific function we will be using is the `metadata` function, which retrieves information about a file or directory.

Let's take a look at a simple code example:

```Rust
use std::fs;

fn main() {
    let directory = "/path/to/directory";

    // Use the metadata function to check if the directory exists
    if let Ok(metadata) = fs::metadata(directory) {
        // Check if the path is a directory
        if metadata.is_dir() {
            println!("Directory {} exists!", directory);
        } else {
            println!("{} is not a directory.", directory);
        }
    } else {
        println!("Directory does not exist.");
    }
}

```
Running this code will output `Directory /path/to/directory exists!` if the directory exists, or `Directory does not exist.` if it does not. 

## Deep Dive
Behind the scenes, the `metadata` function uses the `stat` system call to retrieve the metadata of the file or directory. This includes information such as permissions, ownership, and timestamps. By using the `metadata` function, we can easily access and check this information in our code.

It's also worth noting that the `metadata` function returns a `Result` type. This means that it can return either `Ok(metadata)` if the operation was successful, or `Err(error)` if there was an error. So when using this function, be sure to handle the `Err` case appropriately in your code.

## See Also
- [Rust documentation on std::fs module](https://doc.rust-lang.org/std/fs/index.html)
- [Article on file and directory manipulation in Rust](https://www.tutorialspoint.com/rust/rust_files_io.htm)
- [Example code for checking if a directory exists](https://stackoverflow.com/questions/49593140/how-do-i-check-if-a-directory-exists-in-rust)