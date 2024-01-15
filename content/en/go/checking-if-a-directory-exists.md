---
title:                "Checking if a directory exists"
html_title:           "Go recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Checking if a directory exists is an important task in some programming scenarios. It allows you to ensure that the necessary directories are in place before performing any operations within them, preventing any errors or potential crashes.

## How To

To check if a directory exists in Go, we can use the `os.Stat()` function. This function takes in a path as input and returns an `os.FileInfo` object if the path exists or an error if it doesn't.

```
// Import the os package
import "os"

// Define the path to the directory we want to check
dirPath := "path/to/directory"

// Call the os.Stat() function and check for any errors
if _, err := os.Stat(dirPath); os.IsNotExist(err) {
    // Directory does not exist, handle error here
} else {
    // Directory exists, perform desired operations
}
```

Alternatively, we can use the `os.IsExist()` function which takes in an error and returns a boolean value based on whether the error indicates that the directory exists or not.

```
// Import the os package
import "os"

// Define the path to the directory we want to check
dirPath := "path/to/directory"

// Call the os.Stat() function
if err := os.IsExist(dirPath); err != nil {
    // Directory exists, perform desired operations
} else {
    // Directory does not exist, handle error here
}
```

## Deep Dive

When checking if a directory exists, it is important to handle any errors that may occur. The `os.Stat()` and `os.IsExist()` functions return errors that can provide useful information about why the directory does not exist or if there are any permission issues. It is also possible to use the `os.IsNotExist()` function to specifically check for the "does not exist" error.

Additionally, we can use the `os.Mkdir()` function to create the directory if it doesn't exist. This function also takes in a path as input and can create directories with different levels if needed. However, it is important to note that this function will return an error if the directory already exists, so it should be used carefully.

## See Also

- [os package in Go](https://pkg.go.dev/os)
- [Creating directories in Go](https://golangbot.com/create-directory/)
- [Error handling in Go](https://blog.golang.org/error-handling-and-go)