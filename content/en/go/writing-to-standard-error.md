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

"

## What & Why?
Writing to standard error in Go is a way for programmers to communicate error messages or other important information to the user or other parts of the code. This is done through the standard error stream, which is separate from the standard output stream.

## How to:
To write to standard error in Go, you can use the ```fmt.Fprintln()``` function with the standard error stream as the first argument. For example:
```
Go fmt.Fprintln(os.Stderr, "Error: file not found")
```

This will print the error message to the standard error stream and will not affect the standard output stream. It is important to note that you must import the "fmt" and "os" packages to use this function. 

## Deep Dive:
Writing to standard error is not a new concept and has been used in various programming languages for a long time. It is a standard way of handling error messages and preventing them from interrupting or corrupting the output of a program.

An alternative to writing to standard error in Go is to use the ```log``` package which has functions specifically for writing to standard error. However, the use of ```fmt.Fprintln()``` is a more versatile choice as it allows for formatting and other options.

When writing to standard error, it is important to note that the standard error stream is not buffered, which means that the messages will be printed out immediately without waiting for a newline character. This can be helpful for printing instant error messages or for debugging purposes.

## See Also:
To learn more about writing to standard error in Go, check out the official Go documentation here: https://golang.org/pkg/fmt/ 

You can also explore other packages and functions related to error handling and logging in Go, such as the ```log``` package mentioned earlier.