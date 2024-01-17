---
title:                "Reading command line arguments"
html_title:           "Go recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Reading command line arguments is a way for programmers to pass information or options to their programs at runtime. This allows for flexibility and customization without having to hard code values within the program itself. 

## How to:
To read command line arguments in Go, we can use the built-in `os` package. Here's an example of a simple program that prints out the arguments passed to it:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    args := os.Args[1:] // ignore the first argument, which is the program name itself
    fmt.Println("Arguments:", args)
}
```

If we run this program with the command `go run main.go hello world`, the output would be: `Arguments: [hello world]`. We can also access individual arguments by using indexing, such as `args[0]` for the first argument.

## Deep Dive:
Before the advent of command line arguments, programs would often have to be recompiled or edited to change certain values or behaviors. By using command line arguments, programs can be more dynamic and easily customized without needing to change the code.

Another alternative to using command line arguments is using environment variables. However, this is less convenient and can be more complicated to set up. Furthermore, command line arguments allow for more precise control over a program's behavior.

When reading command line arguments in Go, it's important to handle any potential errors that may occur. For example, if the user does not provide enough arguments, the program may crash. It's also important to handle any unexpected input or data type conversions.

## See Also:
To learn more about reading command line arguments in Go, check out the official Go documentation for the `os` package: https://golang.org/pkg/os/

You can also explore other command line argument parsing libraries in Go, such as `flag` and `cobra`.

Additionally, here's a tutorial on how to build a simple cli tool using Go: https://tutorialedge.net/golang/building-a-cli-in-go/