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

# Why

Have you ever encountered an error or malfunction in your code because a directory that was supposed to exist didn't? Checking if a directory exists can save you from these frustrating situations and ensure that your code runs smoothly.

# How To

Checking if a directory exists in Gleam is a simple process. First, you need to import the `os` package by using the `import os` statement. Then, you can use the `os.exists` function to check if a directory exists. Here's an example:

```Gleam
import os

exists := os.exists("/path/to/directory")
```

The above code will return a Boolean value indicating whether the directory exists or not. You can also use `os.path.exists` for a more detailed check, including checking if the path is a file rather than a directory. Here's an example using `os.path.exists`:

```Gleam
import os

exists := os.path.exists("/path/to/file")
```

The `exists` variable will be set to `True` if the file or directory exists, and `False` if it doesn't.

# Deep Dive

Behind the scenes, `os.exists` and `os.path.exists` actually use the `Sys.File.stat` function to retrieve the information about the specified path. This function returns a `Sys.File.Info` type, which contains information such as the type of the path (file or directory), the file permissions, and more. 

If you want to check for the existence of a directory without importing the `os` package, you can also use the `Sys.File.info` function directly. Here's an example:

```Gleam
import Sys.File

info := Sys.File.info("/path/to/directory")

is_dir := info.type == Sys.File.Dir
```

In the above code, the `info` variable will contain the `Sys.File.Info` type for the specified path, and the `is_dir` variable will be set to `True` if the path is a directory.

# See Also

- [Documentation on os.exists](https://gleam.run/packages/std-lib/file/os.html#method-exists)
- [Documentation on os.path.exists](https://gleam.run/packages/std-lib/file/os.html#method-path-exists)
- [Documentation on Sys.File.info](https://gleam.run/packages/std-lib/file/File.html#method-info)