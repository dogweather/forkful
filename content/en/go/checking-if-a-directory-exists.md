---
title:                "Checking if a directory exists"
date:                  2024-01-20T14:56:28.011050-07:00
html_title:           "Gleam recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"

category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists means confirming whether a specific folder is present on the file system. Programmers do this to prevent errors, like trying to read from or write to a directory that isn't there.

## How to:
Go’s standard library makes it easy. Use `os.Stat` and check for errors with `os.IsNotExist`:

```go
package main

import (
	"fmt"
	"os"
)

func main() {
	dir := "/path/to/your/directory"
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		fmt.Printf("Oops: %v\n", err)
	} else {
		fmt.Println("Yep, it exists!")
	}
}
```

Sample output if the directory doesn't exist:

```
Oops: stat /path/to/your/directory: no such file or directory
```

And if it does:

```
Yep, it exists!
```

## Deep Dive
This "existence check" has been part of Go from the early days, part of the robust `os` package. There's another way: `ioutil.ReadDir` reads the directory and returns an error if it's non-existent. But why bother? It's less efficient for just checking existence.

Under the hood, `os.Stat` does a system call to retrieve the file or directory information. No need to make a call for each file when one will do.

In the past, programmers used to touch a file in the directory, but that's unnecessary I/O. We want efficient and elegant code. Go does this with simplicity.

## See Also
- Go's `os` package documentation: https://pkg.go.dev/os#Stat
- File system operations in Go: https://golang.org/pkg/io/ioutil/#ReadDir
- More about error handling in Go: https://blog.golang.org/error-handling-and-go
