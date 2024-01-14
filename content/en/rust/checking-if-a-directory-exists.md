---
title:    "Rust recipe: Checking if a directory exists"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Why

Rust is a newer programming language that has gained popularity for its focus on memory safety and high performance. When working with file and directory manipulation in Rust, it is important to ensure that the code is robust and can handle any potential errors. One common task in file manipulation is checking if a directory exists before proceeding with any operations. In this blog post, we will explore how to do this in Rust.

## How To

To check if a directory exists in Rust, we can use the `metadata` method from the `std::fs` module. This method returns a `Result` type which can either be `Ok(metadata)` if the directory exists or `Err(error)` if it does not. Let's see an example of how this works:

```Rust
use std::fs;

fn main() {
    let path = "./test_directory";

    // Check if directory exists
    let result = fs::metadata(path);
    match result {
        Ok(metadata) => println!("{} exists", path),
        Err(_) => println!("{} does not exist", path),
    }
}
```

In this example, we first import the `std::fs` module which contains the necessary functions for file and directory manipulation. Then, we declare a variable `path` which holds the path to the directory we want to check. The `fs::metadata` method is used to check if the directory exists and returns a `Result` type. We can use the `match` statement to handle both possible outcomes - `Ok` or `Err`. In the `Ok` case, we print a message indicating that the directory exists, and in the `Err` case, we print a message indicating that it does not exist.

Let's run this code and see the output:

```sh
$ cargo run
./test_directory exists
```

We can also use the `is_dir` method from the `std::fs::Metadata` struct to check if the path is a directory. This method returns a `bool` value of `true` if it is a directory, or `false` if it is not. Here's an example of how to use it:

```Rust
use std::fs;

fn main() {
    let path = "./test_file.txt";

    // Check if path is a directory
    let metadata = fs::metadata(path).unwrap();
    if metadata.is_dir() {
        println!("{} is a directory", path);
    } else {
        println!("{} is not a directory", path);
    }
}
```

In this example, we use the `unwrap` method on the `Result` returned by `fs::metadata` because we are sure that the path given is for a file (not a directory) and we don't need to handle the `Err` case. Then, we use an `if` statement to check if the `metadata` returned is a directory or not. We print the appropriate message based on the result.

Let's run this code and see the output:

```sh
$ cargo run
./test_file.txt is not a directory
```

## Deep Dive

Under the hood, the `fs::metadata` method uses the `stat` system call to get the metadata of the given path. This includes information such as the file's size, permissions, and timestamps. If the given path exists, the `fs::metadata` method will return `Ok` with the metadata. However, if the given path does not exist, it will return `Err` with a `NotFound` error, indicating that the file or directory does not exist.

It is also worth noting that the `fs::metadata` method can be used to check for other types of paths as well, such as files, symbolic links, and even special devices. So, it is a versatile method for checking any type of path.

## See Also

- [Rust Documentation: fs::metadata](https://doc.rust-lang.org/std/fs/fn.metadata.html)
- [Rust Cookbook: Check If a Path Is a Directory](https://rust-lang-nursery.github.io/rust-cookbook/file/dir.html#check-if-a-path-is-a-directory)
- [Rust by Example: Error Handling](https://doc.rust-lang.org/stable/rust-by-example/error/multiple_error_types/try.html)