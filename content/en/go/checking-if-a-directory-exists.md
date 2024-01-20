---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Programmers often need to check if a directory exists to avoid errors when reading or writing files, or when constructing file paths dynamically. Understanding whether a directory exists before interacting with it is a fundamental part of error handling in I/O operations.

## How to:

In Go, we use the `os` package's `Stat` function and `os` package's `IsNotExist` function in conjunction. 

Here's an example:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	dirPath := "/path/to/directory"
	_, err := os.Stat(dirPath)

	if os.IsNotExist(err) {
  		fmt.Printf("Directory %s does not exist.\n", dirPath)
	} else {
  		fmt.Printf("Directory %s exists.\n", dirPath)
	}	
}
```

After you run this code, if the directory exists, then it prints:

``` 
Directory /path/to/directory exists.
```
And if it doesn't:

```
Directory /path/to/directory does not exist.
```

## Deep Dive

The `os.Stat` function retrieves the `FileInfo` structure (which describes a file or directory's metadata). However, if the path does not exist, `os.Stat` will return an error.

In Go's earlier versions, the common way was to check if the error was the exported `os` package's `ErrExist` error type. However, this approach got deprecated because reasons like multiple filesystems and symbolic links complicated error checking. 

Instead, starting Go 1.0, `os.IsNotExist` emerged as the preferred, more accurate method. This function checks if the error from `os.Stat` resembles a non-existent path error across various file systems and link scenarios, adding to error-checking robustness.

Some alternatives include using the `os.Open` function, which also returns an error if the directory doesn't exist. Still, `os.Stat` is more widely used due to its specific use case and performance in terms of system calls.

## See also

- [os package - The Go Programming Language](https://golang.org/pkg/os/)
- [os.Stat function - GoDoc](https://godoc.org/os#Stat)
- [A discussion about different methods of checking file or directory existence in Go on StackOverflow](https://stackoverflow.com/questions/12518876/how-to-check-if-a-file-exists-in-go)