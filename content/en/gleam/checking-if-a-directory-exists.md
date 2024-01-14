---
title:                "Gleam recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

If you're a programmer, you probably know that one of the most common tasks is to access and manipulate files and directories. In order to prevent errors or crashes, it's important to first check if a directory exists before trying to access or modify it. This blog post will guide you through the process of checking for directory existence in Gleam programming, so you can easily implement it in your projects.

## How To
To check for directory existence in Gleam, we will use the `std.fs` library. First, we need to import the library by adding the following code at the top of our file:

```Gleam
import std.fs
```

Next, we will use the `std.fs.exists` function, passing in the path of the directory we want to check. This function returns a `Result` type, which can either be `Ok` or `Err`. In this case, if the directory exists, we will get an `Ok` result, and if it doesn't exist, we will get an `Err` result.

```Gleam
let result = fs.exists("path/to/directory")
```

To handle these possible outcomes, we can use pattern matching and write our code accordingly. Here is an example of how we could handle the results:

```Gleam
case fs.exists("path/to/directory") {
    Ok(_) -> "Directory exists!"
    Err(_) -> "Directory does not exist."
}
```

And that's it! You can now use this code to check for directory existence in your Gleam projects. Below is a sample output of the code in action:

```Gleam
import std.fs

case fs.exists("users/documents") {
    Ok(_) -> "Directory exists!"
    Err(_) -> "Directory does not exist."
}

// Output: "Directory exists!"
```

## Deep Dive
If you want to dive deeper into the topic of checking for directory existence, here are some things to keep in mind:

- The `std.fs.exists` function can also be used to check for file existence, by passing in the path to the file instead of the directory.
- You can also use the `std.fs.metadata` function to get more information about a directory or file, such as its size, last modified date, etc.
- In some cases, checking for directory existence may not be enough. You may also need to check for permissions or other restrictions before being able to access or modify the directory.

## See Also
- [Official documentation for std.fs library](https://gleam.run/libraries/io/)
- [Tutorial on handling errors in Gleam](https://gleam.run/documentation/tutorials/handling-errors)
- [Blog post on manipulating files and directories in Gleam](https://gleam.run/blog/file-operations-in-gleam/)

---

I hope this blog post was helpful in understanding how to check for directory existence in Gleam. With this knowledge, you can now confidently handle file and directory operations in your projects. Happy coding!