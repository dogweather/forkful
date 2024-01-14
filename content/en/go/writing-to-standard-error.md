---
title:                "Go recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

In Go programming, writing to standard error is a common way to log error messages during program execution. This allows developers to easily identify and debug any issues that may arise.

## How To

To write to standard error in Go, we first need to import the `os` package. This package provides functionality for interacting with the operating system.

```Go
import "os"
```

Next, we can use the `os.Stderr` variable to access the standard error output.

```Go
os.Stderr.WriteString("Error message")
```

This will write the given error message to standard error. We can also use the `fmt` package to format the error message before writing it to standard error.

```Go
import "fmt"

//...

errorMsg := "Invalid input: %v"
fmt.Fprintf(os.Stderr, errorMsg, userInput)
```

The above examples will print the error message to the terminal, but we can also redirect the standard error output to a file by using the `os.Stderr` variable along with `os.OpenFile()`.

```Go
file, err := os.OpenFile("error.log", os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
if err != nil {
    fmt.Println("Unable to open error log file.")
    os.Exit(1)
}
defer file.Close()

os.Stderr = file
```

Now, any error messages written to standard error will be redirected to the specified file instead of the terminal.

## Deep Dive

In Go, writing to standard error is a blocking operation. This means that the code execution will pause until the error message is written completely. This is important to keep in mind, especially in concurrent programs.

We can also use the `log` package to write error messages to standard error.

```Go
import "log"

//...

errorMsg := "Error occurred while processing data."
log.Println(errorMsg)
```

This will add a timestamp to the error message before writing it to standard error. Additionally, the `log` package provides multiple levels of severity for errors such as `Print()`, `Panic()`, and `Fatal()`.

## See Also

- [Official documentation for writing to standard error in Go](https://golang.org/pkg/os/#Stderr)
- [Tutorial on using the `log` package in Go](https://www.digitalocean.com/community/tutorials/how-to-use-the-log-package-in-go)
- [Additional information on handling errors in Go](https://blog.golang.org/error-handling-and-go)