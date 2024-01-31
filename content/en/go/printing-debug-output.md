---
title:                "Printing debug output"
date:                  2024-01-20T17:52:53.449140-07:00
model:                 gpt-4-1106-preview
simple_title:         "Printing debug output"

category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Printing debug output is spitting out data to check what your code's up to. Programmers do it to track down bugs or to understand flow and data state at a glance.

## How to:
Here’s how to drop some print lines into your Go code.

```Go
package main

import (
    "fmt"
    "log"
)

func main() {
    // Basic print to stdout
    fmt.Println("Hello, I'm a print statement!")

    // Formatted print
    name, age := "Jane", 28
    fmt.Printf("%s is %d years old.\n", name, age)

    // Printing with log (includes timestamp)
    log.Println("This is a logged info with a timestamp.")

    // For debug, use Printf, but remember to remove later
    debug := true
    if debug {
        fmt.Printf("Debug info: %s is %d years old.\n", name, age)
    }
}
```

Sample output:
```
Hello, I'm a print statement!
Jane is 28 years old.
2009/11/10 23:00:00 This is a logged info with a timestamp.
Debug info: Jane is 28 years old.
```

## Deep Dive:
Historically, `fmt` is Go’s go-to for I/O operations since its inception. It stands for 'format' and gives a bunch of functions to mold text output. `Println` and `Printf` are staples here. `log` package adds time, coherent for tracking events over time.

Alternatives? Sure, beyond basic print statements, you can use logging frameworks like `logrus` or `zap` for structured and leveled logging, perfect for serious applications.

Implementation bits? `fmt` is thread-safe, making your debug prints from concurrent goroutines intelligible. But watch it, debug prints are good for a quick look but could slow you down or make a mess in production code.

## See Also:
- Go by Example on `fmt`: https://gobyexample.com/fmt
- The Go Blog on "Using Go Modules": https://blog.golang.org/using-go-modules (check the part on vended dependencies)
- Go Documentation for `log`: https://pkg.go.dev/log
- Structured logging in Go with `logrus`: https://github.com/sirupsen/logrus
- Blazing fast, structured, leveled logging in Go with `zap`: https://github.com/uber-go/zap
