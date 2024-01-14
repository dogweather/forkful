---
title:                "Go recipe: Checking if a directory exists"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why Checking If a Directory Exists in Go is Important

When working on a project in Go, it is important to check if a directory exists before attempting to access it. This can prevent errors and ensure that your code runs smoothly.

## How To Check If a Directory Exists in Go

To check if a directory exists in Go, we can use the `os.Stat()` function. This function takes in a path as an input and returns a `FileInfo` object. We can then use the `os.IsNotExist()` function to check if the directory exists.

```
Go
func main() {
    // Define the path of the directory to check
    path := "./my_directory"
    
    // Use os.Stat() function to check if the directory exists
    _, err := os.Stat(path)

    // Check if the directory exists
    if os.IsNotExist(err) {
        fmt.Println("Sorry, the directory does not exist")
    } else {
        fmt.Println("The directory exists")
    }
}
```

Running this code will output "The directory exists" if the directory exists or "Sorry, the directory does not exist" if it does not.

## Deep Dive into Checking If a Directory Exists in Go

The `os.Stat()` function returns a `FileInfo` object which contains information about the file or directory it is pointed to. If the file or directory does not exist, an error will be returned instead. This error can be checked using the `os.IsNotExist()` function.

In order to create a directory in Go, we can use the `os.Mkdir()` function. This function takes in the path of the directory to be created and a `FileMode` which sets the permission mode for the directory. If the directory already exists, an error will be returned.

```
Go
func main() {
    // Define the directory to be created
    path := "./my_directory"

    // Attempt to create the directory
    err := os.Mkdir(path, 0777)

    // Check for errors
    if err != nil {
        fmt.Println("Sorry, the directory already exists")
    }
}
```

## See Also

- [Go os package documentation](https://golang.org/pkg/os/)
- [How to check if a file or directory exists in Go](https://www.digitalocean.com/community/tutorials/how-to-check-if-a-file-or-directory-exists-in-go)