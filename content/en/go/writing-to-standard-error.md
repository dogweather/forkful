---
title:    "Go recipe: Writing to standard error"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

If you're new to the world of Go programming, you may have come across the concept of writing to standard error. But what exactly is standard error and why would you want to use it in your code?

In short, standard error is a stream that is used to display error messages or output in a terminal or console. It's a useful tool for debugging and troubleshooting, as it allows you to differentiate between regular output and potential issues within your code.

## How To

To demonstrate how writing to standard error works in Go, let's take a look at a simple code example:

```
package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Println("This is a regular output")

    fmt.Fprint(os.Stderr, "This is an error message")
}
```

In this example, we are using the `fmt` package to print a regular output to the terminal using `Println`. We then use `Fprint` from the same package to print an error message to the standard error stream, denoted by `os.Stderr`.

Executing this code will result in the regular output being printed as normal, but the error message will appear in red text in the terminal, indicating that it is coming from the standard error stream.

## Deep Dive

Now that you understand the basics of writing to standard error, let's dive a bit deeper into how it works.

In Go, the `os` package provides access to standard input, output, and error streams. The standard error stream, `os.Stderr`, is a `File` type and can be used like any other file to read or write data.

If you're dealing with errors in your code, it's best practice to print them to the standard error stream instead of the standard output. This will ensure that any issues are clearly separated from regular output and can easily be identified during debugging.

## See Also

To learn more about standard error and how to use it effectively in your Go code, check out these resources:

- [The official Go documentation on package os](https://golang.org/pkg/os/)
- [A tutorial on standard input, output, and error in Go](https://www.calhoun.io/what-is-stdin-stdout-and-stderr-in-linux/)
- [A blog post discussing error handling in Go](https://blog.golang.org/error-handling-and-go)

Now that you have a better understanding of writing to standard error in Go, you can incorporate it into your code for better error handling and troubleshooting. Happy coding!