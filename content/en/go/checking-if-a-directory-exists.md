---
title:    "Go recipe: Checking if a directory exists"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why 

Do you ever find yourself in a situation where you need to check if a directory exists in your Go program? Perhaps you want to make sure a certain folder exists before trying to access or create files within it. Or maybe you want to verify that a user-provided directory path is valid before proceeding with further operations. Whatever the case may be, knowing how to check for the existence of a directory in Go can come in handy and save you from any potential errors or crashes in your code. 

## How To 

To check if a directory exists in Go, we will be using the standard library's `os` package. Within this package, there is a function called `Stat()` that we can use to retrieve information about a file or directory. We will also be using the `IsNotExist()` function from the same package to check whether the directory does not exist. Let's take a look at an example: 

```Go 
// Checks if a directory exists at the given path 
func directoryExists(path string) bool { 
    // Check if directory path exists, returns error if it doesn't 
    if _, err := os.Stat(path); os.IsNotExist(err) { 
        return false 
    } 
    return true 
} 

func main() { 
    // Example usage 
    fmt.Println(directoryExists("/Users/john/Desktop")) 
    // Output: true 
    fmt.Println(directoryExists("/Users/john/Documents")) 
    // Output: false 
} 
``` 

In this example, we define a function `directoryExists()` that takes in a directory path as a parameter and returns a boolean value indicating whether the directory exists or not. Within the function, we use `os.Stat()` to retrieve information about the directory at the given path. If the directory does not exist, this function will return an error and we can use `os.IsNotExist()` to check if the error is a "not exist" error. If so, we return false, indicating that the directory does not exist. Otherwise, we return true, indicating that the directory does exist. 

## Deep Dive 

The `os.Stat()` function not only checks for the existence of a directory but also returns additional information about the directory, such as its size, permissions, and modification time. This can be useful if you need to access this information for further operations in your program. Additionally, instead of using `os.IsNotExist()`, you can also use `os.IsExist()` to check if a directory already exists and `os.IsPermission()` to check if there are any permission errors in accessing the directory. 

It's also important to note that `os.Stat()` and `os.IsNotExist()` will also work with file paths, not just directories. So if you need to check for the existence of a file, you can use the same approach as above. 

## See Also 

- [Go Documentation - os package](https://golang.org/pkg/os/) 
- [A Tour of Go - File Stat](https://tour.golang.org/basics/11) 
- [Checking if a file exists in Go](https://www.calhoun.io/how-to-check-if-a-file-or-directory-exists-in-go/)