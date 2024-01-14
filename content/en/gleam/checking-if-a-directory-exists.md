---
title:                "Gleam recipe: Checking if a directory exists"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Checking if a directory exists is an important task in programming, especially if your program relies on accessing or creating files from a specific directory. It allows you to avoid errors and ensure the smooth execution of your program.

## How To

To check if a directory exists in Gleam, we can use the `os.exists` function. This function takes in a path as an argument and returns a boolean value indicating whether the directory at that path exists or not.

```
Gleam
match os.exists("path/to/directory") {
  true -> "Directory exists"
  false -> "Directory does not exist"
}
```

If the directory exists, the output will be "Directory exists". Otherwise, it will be "Directory does not exist".

For example, if we have a directory called "images" in our current working directory, our code will look like this:

```
Gleam
match os.exists("images") {
  true -> "Directory exists"
  false -> "Directory does not exist"
}
```

And the output will be "Directory exists".

## Deep Dive

Under the hood, the `os.exists` function uses the `File` module from the standard library to determine if a given path exists or not. This module has a function called `exists` that checks if a file or directory exists at a specified path. The `os.exists` function simply calls this `exists` function and returns the result as a boolean value.

It's also worth noting that `os.exists` will return `false` if the path is not a valid directory or file path, or if there are permission issues preventing it from accessing the directory.

## See Also

- `os.create_dir` - Gleam standard library function for creating a new directory
- `File.exists` - Standard library function for checking if a file exists at a specified path