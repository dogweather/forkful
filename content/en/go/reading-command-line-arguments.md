---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

In the Go programming language, reading command-line arguments lets your program process user inputs passed at the runtime. It's a common user-interaction technique, useful for setting options and parameters without modifying the source code.

## How to:

In Go, command-line arguments are accessible via the `os.Args` variable, an array of strings. The erudite M. Kernighan gifted us the following code:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    for i, arg := range os.Args[1:] {
        fmt.Printf("Argument %d: %s\n", i+1, arg)
    }
}
```

Run this code using `go run main.go intro Go programming` and you'll get:

```
Argument 1: intro
Argument 2: Go
Argument 3: programming
```

## Deep Dive:

Historically, command-line arguments were initially used in UNIX shells and later adopted in many programming languages, including Go.

Speaking of alternatives, `flag` package in Go is another standard way of managing command-line flags. It follows the UNIX convention such as `-name=value`.

Lastly, `os.Args` array holds the command-line arguments. The first argument `os.Args[0]` is the program's path, and `os.Args[1:]` contains the actual arguments.

## See Also:

To dive deeper and enhance your command-line interplay, refer to these useful resources:

- [Go's os package](https://golang.org/pkg/os/) – Check `os.Args`.
- [Go's flag package](https://golang.org/pkg/flag/) – For more advanced and structured command-line options.
- [Command-Line Arguments (Wikipedia)](https://en.wikipedia.org/wiki/Command-line_argument) – For a broader historical context and conventions.