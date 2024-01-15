---
title:                "Writing to standard error"
html_title:           "Go recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to standard error, also known as stderr, is a common practice in programming. It allows developers to print out error messages and other important information that can help with debugging and troubleshooting. Additionally, writing to stderr ensures that these messages are separate from regular output, making them easier to identify and handle.

## How To

To write to stderr in Go, we can use the `fmt.Fprintf()` function and specify `os.Stderr` as the first argument. Let's take a look at an example code:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Fprintf(os.Stderr, "This is an error message.")
}
```

Running this code will print the message "This is an error message." to the standard error output. We can also format the message as we would with `fmt.Printf()`, adding variables and other information as needed.

We can also use `fmt.Errorf()` to create a formatted error message and write it directly to stderr:

```Go
// ...
err := fmt.Errorf("An error occurred: %d", 404)
fmt.Fprintln(os.Stderr, err)
```

This will print the following to the standard error output:

```
An error occurred: 404
```

## Deep Dive

Internally, standard error is represented by a file descriptor, similar to standard input and output. By default, stderr is associated with the console or terminal where the program is executed. However, we can redirect stderr to a file or another output stream for better error handling.

We can also use the `log` package in Go to write to both stdout and stderr simultaneously. This can be helpful for printing informative messages to both streams while also capturing errors in the log file.

## See Also

- [Official fmt Package Documentation](https://golang.org/pkg/fmt/)
- [Error Handling in Go](https://blog.golang.org/error-handling-and-go)
- [Logging in Go with Logrus](https://www.loggly.com/blog/logging-with-golang-part-one/)