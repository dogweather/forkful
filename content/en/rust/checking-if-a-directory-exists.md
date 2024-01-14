---
title:    "Rust recipe: Checking if a directory exists"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered a situation in your Rust programming where you need to check if a directory exists? Many developers may wonder why this is necessary, especially if they are used to other languages where directory creation is automatically handled. However, in Rust, checking if a directory exists is important for ensuring proper file handling and preventing errors in your code. In this blog post, we will explore how to check if a directory exists in Rust and why it is beneficial to do so.

## How To

Let's dive right into some coding examples to see how we can check if a directory exists in Rust.

```Rust
use std::fs;

let directory = "/my_directory";
if fs::metadata(directory).is_ok() {
    println!("The directory exists!");
} else {
    println!("The directory does not exist.");
}
```

In this example, we first import the `fs` module from the standard library in order to use the `metadata` function. Next, we provide the path of the directory we want to check. This path is then passed as an argument to the `metadata` function, which returns a `Result` type. We can then use the `is_ok` method to check if the directory exists or not, and print an appropriate message to the user.

We can also use the `Path` struct from the `std::path` module to do the same check:

```Rust
use std::path::Path;

let directory = Path::new("/my_directory");
if directory.exists() {
    println!("The directory exists!");
} else {
    println!("The directory does not exist.");
}
```

Using the `exists` method on a `Path` object will also return a `bool` value depending on the existence of the directory.

## Deep Dive

Now, let's take a deeper look into what happens when we perform a check for a directory's existence in Rust. Behind the scenes, the `metadata` function calls the operating system's metadata system call to retrieve information about the directory. If the call returns an error, it means that the directory does not exist or there was some other issue with accessing it. This is why the `metadata` function returns a `Result` type, as there can be different types of errors that could occur.

Using the `Path` struct's `exists` method also calls the same system call and returns the same `Result` type, but it is handled internally, giving a cleaner and more straightforward code.

## See Also

- Rust Standard Library Documentation for `fs` module: [https://doc.rust-lang.org/std/fs/](https://doc.rust-lang.org/std/fs/)
- Rust Standard Library Documentation for `std::path` module: [https://doc.rust-lang.org/std/path/](https://doc.rust-lang.org/std/path/)
- Rust Book - Working with Files: [https://doc.rust-lang.org/book/working-with-files.html](https://doc.rust-lang.org/book/working-with-files.html)