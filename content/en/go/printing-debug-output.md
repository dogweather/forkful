---
title:                "Go recipe: Printing debug output"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debug output is an essential part of the development process for any programmer. It allows us to understand the flow of our code and identify any potential errors or bugs. In the Go programming language, there are various methods for printing debug output, each with its own advantages and use cases.

## How To

To print debug output in Go, we can use the built-in `fmt` package, which provides functions for formatting and printing data. Let's take a look at some examples:

```Go
package main

import "fmt"

func main() {
    // printing a single variable
    name := "John"
    fmt.Println("Hello", name)

    // formatting output
    num1 := 10
    num2 := 20
    fmt.Printf("The sum of %v and %v is %v\n", num1, num2, num1+num2)

    // printing multiple variables
    age := 25
    location := "New York"
    fmt.Println("I am", age, "years old and currently living in", location)
}
```

Output:
```
Hello John
The sum of 10 and 20 is 30
I am 25 years old and currently living in New York
```

As we can see in the examples, we can use `Println` to simply print the variables, `Printf` to format the output, and `Print` for printing without adding a newline at the end. Additionally, the `%v` verb in `Printf` allows us to print any type of variable.

## Deep Dive

Apart from the `fmt` package, there are other ways to print debug output in Go. One popular method is using the `log` package, which provides functions for logging data and displaying it in a structured manner. It also has different levels of logging, such as `Info`, `Warning`, and `Error`, which can help us differentiate the significance of the printed data.

Another technique is using `panic` and `recover` to handle errors and print out relevant information when necessary. This can be useful for debugging code that is prone to unexpected runtime errors.

Another important aspect of printing debug output is the use of `defer` statements. These statements allow us to schedule a function call to be executed at the end of the current function, which is helpful for printing data after the execution of a block of code. It also allows us to handle errors more efficiently by printing relevant information before exiting the function.

## See Also

- Official documentation for the `fmt` package: https://golang.org/pkg/fmt/
- Official documentation for the `log` package: https://golang.org/pkg/log/
- Using `panic` and `recover` for error handling in Go: https://blog.golang.org/defer-panic-and-recover