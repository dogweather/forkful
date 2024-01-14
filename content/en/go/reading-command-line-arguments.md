---
title:    "Go recipe: Reading command line arguments"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Command line arguments are an essential part of many programs, enabling users to provide inputs and customize their experience. Learning how to read command line arguments can greatly enhance your Go programming skills and make your programs more versatile.

## How To

Reading command line arguments in Go is a straightforward process. First, we need to import the `os` package, which provides functions for interacting with the operating system. Then, we can use the `os.Args` variable to access the command line arguments.

To demonstrate this, let's create a simple program that takes in two arguments and outputs them. We can start by creating a new file `args.go` and adding the following code:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // os.Args[0] is the name of the program
    // os.Args[1:] contains all the arguments
    fmt.Println("First argument:", os.Args[1])
    fmt.Println("Second argument:", os.Args[2])
}
```

Now, we can compile and run our program using the following commands:

```
go build args.go
./args hello world
```

The output should look like this:

```
First argument: hello
Second argument: world
```

We can also use a `for` loop to access each argument individually, like this:

```Go
for i := 0; i < len(os.Args); i++ {
    fmt.Printf("Argument %d: %s\n", i, os.Args[i])
}
```

Try running this program with different arguments and see how it behaves.

## Deep Dive

Now, let's take a deeper look at how the `os.Args` variable works. It is a slice of strings, which means that we can use all the functions and methods available for manipulating slices. For example, we can use the `len()` function to get the number of arguments, and the `append()` function to add more arguments to the slice.

We can also use the `flag` package to parse command line arguments more efficiently. This package provides functions for defining flags and getting their values easily. You can explore the `flag` package further on your own to see how it can improve your command line argument handling.

## See Also

For more information on the `os` and `flag` packages, check out the following links:

- [os package documentation](https://golang.org/pkg/os/)
- [flag package documentation](https://golang.org/pkg/flag/)
- [Go by Example: Command-Line Arguments](https://gobyexample.com/command-line-arguments)

Happy coding!