---
title:    "Go recipe: Printing debug output"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of the software development process, and being able to print out relevant information about the program's execution can be incredibly useful. Whether it's for troubleshooting a specific issue or gaining a better understanding of the code flow, printing debug output is a powerful tool for developers.

## How To

To print debug output in Go, we can use the built-in `fmt` package's `Println` function. It takes in any number of arguments and prints them to the console.

```Go
fmt.Println("Hello World!")
```

This will print "Hello World!" to the console. We can also use string interpolation to print out the value of a variable.

```Go
name := "John"
fmt.Println("Hello", name)
```

This will print "Hello John". We can also use the `Printf` function to format our output.

```Go
age := 25
fmt.Printf("I am %d years old.", age)
```

This will print "I am 25 years old."

## Deep Dive

The `fmt` package offers various other functions for printing debug output, such as `Sprintf` and `Fprintln`. These functions give us more control over the format and destination of our output. We can also use the `%v` verb in `Printf` to print any value in its default format.

Additionally, we can use the `log` package for more advanced logging capabilities. It allows us to specify the severity level of the message and include a timestamp. We can also create our own custom logger with specific configurations.

## See Also

- [fmt package documentation](https://golang.org/pkg/fmt/)
- [log package documentation](https://golang.org/pkg/log/)
- [Debugging in Go: Tips and Tricks](https://medium.com/swlh/debugging-in-go-tips-and-tricks-5ae5087b5c88)