---
title:                "Rust recipe: Checking if a directory exists"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why
In programming, it's important to check if a directory exists before attempting to access or create files within it. This ensures that the program runs smoothly and avoids any errors that may occur.

## How To
To check if a directory exists in Rust, we can use the `Path::is_dir()` method from the `std::fs` module. This method returns a `bool` value of `true` if the specified path exists and is a directory, and `false` if it doesn't exist or is not a directory.

Let's take a look at an example code snippet:
```rust
use std::fs;

let path = "new_directory";

if fs::Path::new(path).is_dir() {
    println!("The directory {} exists!", path);
} else {
    println!("The directory {} does not exist.", path);
}
```
In this code, we first import the `fs` module from the standard library. Then, we define a variable `path` with the name of the directory we want to check. Using `fs::Path::new()` we create a `Path` object from the specified path. Finally, we use the `is_dir()` method to check if the path exists as a directory. Depending on the result, we print out a corresponding message.

If we run this code and the directory `new_directory` exists, the output will be:
```
The directory new_directory exists!
```

## Deep Dive
In addition to the `is_dir()` method, the `Path` struct in Rust also has other methods such as `exists()`, `is_file()`, and `metadata()` that can provide more information about the specified path. These methods can be useful for further file and directory manipulation.

It's also worth mentioning that the `Path` struct can handle both absolute and relative paths, making it versatile for different use cases. Furthermore, the methods from the `fs` module can also handle platform-specific path differences, making it easier to write cross-platform code.

## See Also
- [Official Rust documentation for checking if a path exists](https://doc.rust-lang.org/std/fs/struct.Path.html#method.is_dir)
- [Rust Cookbook on manipulating files and directories](https://rust-lang-nursery.github.io/rust-cookbook/file/dir.html)
- [Tutorialspoint's Rust tutorial on working with files and directories](https://www.tutorialspoint.com/rust/rust_files_io.htm)

With these resources, you can further explore and enhance your knowledge of handling directories in Rust. Happy coding!