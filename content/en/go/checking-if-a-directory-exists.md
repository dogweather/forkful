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

## What & Why?

Checking if a directory exists is a common task in programming. It involves verifying whether a certain directory (or folder) exists in a specified path. This is important because it allows developers to perform different actions based on whether the directory exists or not. For example, if the directory exists, the program may continue with its normal execution, but if it doesn't exist, the program may create the directory or handle the error in a specific way.

## How to:

### Example 1:
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// Specify a directory path
	path := "C:/Users/John/Documents"
	
	// Check if directory exists using os.Stat() function
	// This returns an error if the directory doesn't exist
	if _, err := os.Stat(path); err == nil {
		// Directory exists
		fmt.Println("Directory exists!")
	} else {
		// Handle the error
		fmt.Println("Directory doesn't exist. Error:", err)
	}
}
```

#### Output:
```
Directory exists!
```

### Example 2:
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// Specify a directory path
	path := "C:/Users/John/Downloads"
	
	// Check if directory exists using os.IsExist() function
	// This returns a boolean value instead of an error
	if _, err := os.Stat(path); os.IsExist(err) {
		// Directory exists
		fmt.Println("Directory exists!")
	} else {
		// Handle the error
		fmt.Println("Directory doesn't exist. Error:", err)
	}
}
```

#### Output:
```
Directory exists!
```

## Deep Dive:

Checking if a directory exists has been a common programming task for a long time. The need to handle situations where a directory may or may not exist has always been present in various systems and applications. In the past, different programming languages had different ways of checking for the existence of a directory. However, with the rise of modern languages like Go, this task has become much easier and standardized.

There are several ways to check for the existence of a directory in Go. One option is to use the os.Stat() function, which returns information about a file or directory, including an error if the file or directory does not exist. Another option is to use the os.IsExist() function, which returns a boolean value instead of an error. Both of these functions are part of the standard library in Go.

## See Also:

- [os.Stat() function documentation](https://golang.org/pkg/os/#Stat)
- [os.IsExist() function documentation](https://golang.org/pkg/os/#IsExist)