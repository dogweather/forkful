---
title:                "Go recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why
Debug output is an essential tool for any programmer. It allows you to see the inner workings of your code and helps you identify any errors or bugs that may be causing issues. By printing debug output, you can better understand the flow of your program and make informed decisions on how to improve it.

## How To
To print debug output in Go, you can use the `fmt` package's `Println()` function. This function takes in any number of arguments and prints them out in the specified format. For example:

```Go
age := 25
name := "John"
fmt.Println("Name:", name, "Age:" age)
```

This will output: `Name: John Age: 25` to the console. You can also use the `Sprintf()` function to store the debug output in a string variable for later use. For more complex debug output, you can also use the `Printf()` function to format the output using verbs, such as `%v` for any value, `%d` for integers, or `%s` for strings.

```Go
num := 3.14
str := "Hello"
fmt.Printf("Value: %v String: %s", num, str)
```

This will output: `Value: 3.14 String: Hello`. By using these functions, you can easily customize and print out debug output to track variables, function calls, and more.

## Deep Dive
To take the printing of debug output to the next level, you can use the `log` package. This package provides more advanced functionality for debugging, such as setting different levels of output (such as `Print`, `Info`, `Warn`, and `Error`), adding timestamps to the output, and even writing the output to a log file. This is especially useful for larger projects where the console output can get cluttered, but you still need to keep track of important information.

Additionally, you can use conditional statements to only print debug output when a specific condition is met. This can help reduce unnecessary clutter in your output and make it easier to pinpoint any errors.

## See Also
- Official `fmt` package documentation: https://golang.org/pkg/fmt/
- Official `log` package documentation: https://golang.org/pkg/log/
- "Debugging in Go using the fmt package" article: https://blog.alexellis.io/golang-debugging-hello-world/