---
date: 2024-02-03 17:50:04.449226-07:00
description: "How to: Go provides direct access to command-line arguments through\
  \ the `os` package, specifically using `os.Args`, an array of strings. Here\u2019\
  s a simple\u2026"
lastmod: '2024-03-13T22:44:59.644860-06:00'
model: gpt-4-0125-preview
summary: Go provides direct access to command-line arguments through the `os` package,
  specifically using `os.Args`, an array of strings.
title: Reading command line arguments
weight: 23
---

## How to:
Go provides direct access to command-line arguments through the `os` package, specifically using `os.Args`, an array of strings. Here’s a simple example to get us started:

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // os.Args provides access to raw command-line arguments
    fmt.Println("Command-line arguments:", os.Args)

    if len(os.Args) > 1 {
        // Loop through arguments, skipping the first one (program name)
        for i, arg := range os.Args[1:] {
            fmt.Printf("Argument %d: %s\n", i+1, arg)
        }
    } else {
        fmt.Println("No command-line arguments provided.")
    }
}
```

Sample output when run with `go run yourprogram.go arg1 arg2` might look like:

```
Command-line arguments: [/tmp/go-build123456789/b001/exe/yourprogram arg1 arg2]
Argument 1: arg1
Argument 2: arg2
```

This prints all the arguments including the program name (often at index 0), then iterates over each argument provided, printing them out. For more controlled argument parsing, you might consider the `flag` package for parsing command-line options.

## Deep Dive
Historically, accessing command-line arguments is a practice as old as C programming, where `argc` and `argv[]` serve a similar purpose. In Go, `os.Args` makes it straightforward but deliberately rudimentary. For more complex scenarios, such as handling flags or options, Go offers the `flag` package which provides robust parsing capabilities. This could be seen as a "better" alternative when your application requires more than just positional arguments.

Unlike some scripting languages that offer built-in parsing of command-line arguments into associative arrays or objects, Go's approach requires programmers to either handle parsing manually using `os.Args` for basic needs or to leverage the `flag` package for more advanced scenarios. This design reflects Go's philosophy of keeping the core language simple while providing powerful standard libraries for common tasks. While it may introduce a slight learning curve for those accustomed to built-in parsing, it offers greater flexibility and encourages a deeper understanding of command-line argument handling.
