---
title:                "Go recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Why Check If a Directory Exists in Go

When working with files and directories in a Go program, it is important to check if a directory exists before attempting to access or manipulate it. This ensures that your code runs smoothly without any unexpected errors.

# How To Check If a Directory Exists in Go

Checking if a directory exists in Go is a simple task. It can be done using the built-in `os.Stat()` function, which returns information about a file or directory. We can use this function to check if a given path exists and is a directory.

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    path := "./my_directory" // path of directory to check
    info, err := os.Stat(path) // get information about path
    if err != nil { // check if error occurred
        if os.IsNotExist(err) { // check if path does not exist
            fmt.Printf("Directory %s does not exist.", path)
        } else {
            fmt.Printf("Error checking directory: %s", err)
        }
    } else if info.IsDir() { // check if path is a directory
        fmt.Printf("Directory %s exists.", path)
    } else {
        fmt.Printf("%s is not a directory.", path)
    }
}
```

The above code first assigns a path to a variable `path`. In this case, we are checking for a directory named `my_directory` in the current directory. Next, the `os.Stat()` function is used to get information about the given path. If an error occurs, we check if it is because the path does not exist or if it is another type of error. If the path exists and is a directory, we print a message confirming its existence. Otherwise, we print that it is not a directory.

Sample output for a directory that exists: `Directory ./my_directory exists.`

Sample output for a non-existent directory: `Directory ./my_unknown_directory does not exist.`

Sample output for a file instead of a directory: `./file.txt is not a directory.`

# Deep Dive into Checking If a Directory Exists in Go

Apart from the `os.Stat()` function, there are other ways to check if a directory exists in Go. One method is by using the `os.IsExist()` function, which checks if a file or directory exists. Another approach is to use the `os.Open()` function, which attempts to open a file or directory and returns an `os.File` object, which can be used to get information about the path.

Additionally, the `path/filepath` package has a `Walk()` function that allows for walking through a directory and checking if it exists. This is useful when working with nested directories.

It is also important to take into consideration scenarios where the program might not have permission to access the directory. In such cases, an error will be returned even if the directory exists. Therefore, it is good practice to check for both errors and the existence of the directory.

# See Also

- [Go Documentation on os.Stat()](https://golang.org/pkg/os/#Stat)
- [Go Documentation on filepath.Walk()](https://golang.org/pkg/path/filepath/#Walk)
- [Medium Article on Checking If a File or Directory Exists in Go](https://medium.com/@chlumberg/creating-folders-in-go-dc9eb4eadb13)