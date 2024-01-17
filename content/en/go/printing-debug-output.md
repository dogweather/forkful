---
title:                "Printing debug output"
html_title:           "Go recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Debugging is an essential part of programming. To understand and fix errors in your code, you need to see what is happening as it runs. Printing debug output allows you to display information in your program as it is being executed. This helps you identify and diagnose errors, making your code more efficient and reliable.

## How to:
Go provides a built-in package called `fmt` that allows you to print debug output. Let's look at some examples of how to use it:
```
Go using fmt

// Print a single value
fmt.Println("Hello World!")

// Print multiple values
fmt.Println("Hello,", "World!", "How", "are", "you?")

// Print values with formatting
fmt.Printf("The value of x is %d\n", 10)
```
Output:
```
Hello World!
Hello, World! How are you?
The value of x is 10
```

## Deep Dive:
Before the `fmt` package existed, programmers would use `println()` to print debug output. However, this function only accepts a single argument and doesn't allow for formatting. The `fmt` package was introduced in Go to provide a more versatile and convenient way to print output.

There are also alternative options for printing debug output in Go, such as using the `log` package or a third-party logging library like `logrus` or `zap`. These options may offer additional features or customization, but the `fmt` package is sufficient for most debugging needs.

Internally, the `fmt` package uses the `io.Writer` interface to send output to the standard output stream, which by default is your terminal. This allows the `fmt` package to also be used for logging to files or other streams.

## See Also:
- [Go fmt Package Documentation](https://pkg.go.dev/fmt)
- [Go Log Package Documentation](https://pkg.go.dev/log)
- [Logrus Package](https://github.com/sirupsen/logrus)
- [Zap Package](https://github.com/uber-go/zap)