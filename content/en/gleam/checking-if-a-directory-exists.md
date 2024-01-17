---
title:                "Checking if a directory exists"
html_title:           "Gleam recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists is a common task in programming where we want to verify whether a given directory exists on the file system or not. This is essential because it allows us to handle situations where the directory may have been deleted or moved. This check is necessary for the smooth functioning of our code and to ensure that we are accessing the correct directory.

## How to:

To check if a directory exists in Gleam, we can use the `dir::exists` function from the `gleam/file` module. This function takes in a path to the directory as its argument and returns a boolean value indicating whether the directory exists or not.

Let's see an example:

```Gleam
import gleam/file

pub fn main() {
  let dir = "path/to/directory/"
  if dir::exists(dir) {
    println("Directory exists!")
  } else {
    println("Directory does not exist!")
  }
}
```

If the directory exists, the output will be `Directory exists!`. Otherwise, it will print `Directory does not exist!`.

## Deep Dive:

In the earlier days of programming, directory checking was usually done by trying to open the directory and checking for any errors. However, this approach was not efficient as it involved a lot of back and forth communication with the file system. Newer languages and libraries like Gleam have built-in functions for this task, making it more efficient and convenient.

An alternative approach to checking if a directory exists is by using the `std::fs` module from the standard library. This module provides the `metadata` function which can be used to retrieve information about a file or directory. However, the approach using `gleam/file` is more straightforward and recommended.

When implemented, the `dir::exists` function uses the `Path.exists` method from the Rust standard library, which ultimately uses the `access` system call to check if the directory exists.

## See Also:

- [`gleam/file` module documentation](https://gleam.run/libraries/file/)
- [Gleam standard library `std::fs` module documentation](https://gleam.run/stdlib/fs/)
- [Rust `Path.exists` method documentation](https://doc.rust-lang.org/std/path/struct.Path.html#method.exists)