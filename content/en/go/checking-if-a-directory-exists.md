---
title:    "Go recipe: Checking if a directory exists"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why 

Checking if a directory exists may seem like a simple task, but it plays an important role in many programs. It allows us to ensure that our code runs smoothly and avoids errors, which saves us time and frustration in the long run. In this blog post, we will explore how to check if a directory exists in Go and why it is crucial for our programming needs. 

## How To 

In Go, we can use the built-in `os.Stat()` function to check if a directory exists. Let's take a look at a simple code example: 

```Go
package main 

import (
	"fmt"
	"os"
)

func main() {
	// check if directory exists
	dir := "my_directory"
	if _, err := os.Stat(dir); !os.IsNotExist(err) {
		fmt.Println(dir, "exists!")
	} else {
		fmt.Println(dir, "does not exist!")
	}
}
```

In this example, we use the `os.Stat()` function to check if the directory "my_directory" exists. The function returns an `os.FileInfo` struct, which contains information about the file or directory. We then use the `os.IsNotExist()` function to check if the error returned is due to the directory not existing. If it does not exist, we print out a message indicating that it does not exist. 

We can also use the `os.Mkdir()` function to create a directory if it does not already exist. Let's take a look at another code example: 

```Go
package main 

import (
	"fmt"
	"os"
)

func main() {
	// create directory if it does not exist
	dir := "new_directory"
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		err := os.Mkdir(dir, 0755)
		if err != nil {
			fmt.Println("Error creating directory!")
		} else {
			fmt.Println("Directory created successfully!")
		}
	} else {
		fmt.Println(dir, "already exists!")
	}
}
```

In this code, we use the same `os.Stat()` function to check if the directory "new_directory" exists. If it does not, we use the `os.Mkdir()` function to create it with the appropriate permissions. By checking if a directory exists before creating it, we avoid overwriting existing directories and ensure our code runs smoothly. 

## Deep Dive 

Under the hood, the `os.Stat()` function uses system calls to retrieve information about a file or directory. It returns an `os.FileInfo` struct which contains information about the size, permissions, and modification time of the file or directory. In the case of a directory, the `os.FileInfo` struct will also contain information about any subdirectories or files within that directory. 

It is also worth mentioning that the `os.Stat()` function will follow symbolic links, so if a link points to a valid directory, it will return true even if the symlink itself does not exist. 

## See Also 

- [os.Stat() Go documentation](https://pkg.go.dev/os#Stat)
- [os.Mkdir() Go documentation](https://pkg.go.dev/os#Mkdir)
- [Understanding file permissions in Go](https://www.digitalocean.com/community/tutorials/understanding-file-permissions-in-go)