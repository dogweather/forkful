---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Debugging output refers to the data, from print statements, that aids in identifying, isolating, and fixing software issues. It's done simply because it's a vital part of programming: coders need feedback on how their script performs and to rectify mishaps.

## How to:

In Go, the fmt package provides handy functions for printing debug output. Here's a simple example:

```Go
package main

import "fmt"

func main() {
    fmt.Println("Hello, Go!")
}
```
Upon running, our program will output:
```
Hello, Go!
```
Now, for a debugging case we often print the values of variables:

```Go
package main

import "fmt"

func main() {
    var a = 20
    var b = 30
    fmt.Println("a:", a, "b:", b)
}
```
Output would be:

```
a: 20 b: 30
```

## Deep Dive

Historically, programming languages have varied in their debugging approaches; some automatically print debugging information when conditions are met, while others, including Go, leave it up to the programmer as and when needed. 

As an alternative to fmt for debugging, one could use `log` package or third-party tools like Delve for more advanced debugging operations.

The implementation of fmt's print functions in Go convert various data types to a string representation that is human-readable. It's important to note that print statements slow down a program and should ideally not make their way to a production environment.

## See Also:

- Official Go docs on the fmt package: [golang.org/pkg/fmt/](https://golang.org/pkg/fmt/)
- In-depth video on Go debugging: [youtube.com/watch?v=HsQxctfT-8Y](https://www.youtube.com/watch?v=HsQxctfT-8Y)
- Delve, a debugger for the Go programming language: [github.com/derekparker/delve](https://github.com/go-delve/delve)