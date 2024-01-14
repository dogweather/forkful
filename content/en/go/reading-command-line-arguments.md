---
title:                "Go recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why
When writing command-line applications in Go, understanding how to read command line arguments is crucial. This allows you to provide more flexibility and options to the end user, making your application more user-friendly.

## How To
To read command line arguments in Go, we first need to import the `os` package. This package provides functions for interacting with the operating system.

```Go
import (
    "fmt"
    "os"
)
```

Next, we can use the `Args` function from the `os` package to get a slice of all the command line arguments passed to our application.

```Go
arguments := os.Args
```

We can then use a for loop to iterate through the slice and print out each argument.

```Go
for i := 0; i < len(arguments); i++ {
    fmt.Println(arguments[i])
}
```

If we run our application with the command `go run main.go arg1 arg2`, the output will be:

```
main.go
arg1
arg2
```

We can also access individual arguments by index, similar to how we access elements in a regular slice. For example, to get the first argument, we would use `arguments[0]`.

## Deep Dive
The `Args` function returns a slice of strings, where each element corresponds to a command line argument. It's worth noting that the first element in the slice (`arguments[0]`) will always be the name of our application.

We can also use the `Flags` function from the `flag` package to parse flags and their values from the command line. This allows us to provide more specific instructions to our application, such as providing a file path or setting a verbose mode.

```Go
import (
    "flag"
)

// create a string flag for a file path
pathPtr := flag.String("file", "", "specify a file path")
// create a bool flag for verbose mode
verbosePtr := flag.Bool("verbose", false, "enable verbose mode")

// parse all the flags
flag.Parse()

// access the values of the flags
path := *pathPtr
verbose := *verbosePtr
```

With this, we can now run our application with `go run main.go -file test.txt -verbose` and access the file path and verbose mode in our code.

## See Also
- [Command Line Arguments in Go](https://gobyexample.com/command-line-arguments)
- [os package documentation](https://golang.org/pkg/os/)
- [flag package documentation](https://golang.org/pkg/flag/)