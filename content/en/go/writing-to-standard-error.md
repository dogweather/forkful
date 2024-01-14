---
title:                "Go recipe: Writing to standard error"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to standard error is a common practice in Go programming for troubleshooting and debugging purposes. It allows developers to print out error messages and other valuable information that can aid in identifying and fixing issues within their code.

## How To

To write to standard error in Go, we can use the `fmt.Fprintf()` function with `os.Stderr` as the first argument. This allows us to format and print an error message to the standard error stream.

```
Go fmt.Fprintf(os.Stderr, "Error: %v", err)
```

The `os.Stderr` command is used to specify the output stream as the standard error, rather than the standard output. This ensures that our error message is printed to the appropriate stream, making it easier to distinguish from other printed statements.

A simple example of how this can be used is in error handling. For instance, we can use `fmt.Fprintf` to print out a custom error message and the actual error when an error occurs in our code. This can help us pinpoint the exact cause of the error and make it easier to fix.

```
err := doSomething()
if err != nil {
  fmt.Fprintf(os.Stderr, "Failed to do something: %v", err)
  // Other error handling logic
}
```

## Deep Dive

In Go, standard error is typically used for printing out error messages and other critical information that may not necessarily need to be seen by the end-user. Unlike the standard output, which is usually displayed on the console or terminal, standard error is typically redirected to a log file or a debugging tool.

Additionally, writing to standard error allows for the use of formatting and string interpolation, making error messages more informative and helpful. By specifying the stream as standard error, we can also distinguish between regular output and error messages, making it easier to filter and debug our code.

## See Also

- [Printing to Standard Error in Go](https://golang.org/pkg/fmt)
- [The Standard Library Package os](https://golang.org/pkg/os)
- [Error Handling in Go](https://blog.golang.org/error-handling-and-go)