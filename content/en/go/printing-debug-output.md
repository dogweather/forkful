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

## Why
Debugging is an important process in any programming language. It helps identify and fix errors in code, ensuring that the program runs smoothly. One way to facilitate the debugging process is by printing debug output. This allows developers to track the flow of their code and pinpoint specific areas that may be causing problems.

## How To

To print debug output in Go, we can use the `fmt` package. This package provides functions for formatted input and output, including printing to the console. Let's take a look at a simple example:

```Go
package main

import "fmt"

func main() {
  name := "John"
  age := 25
  fmt.Printf("Hello, my name is %s and I am %d years old", name, age)
}
```

Running this code will output the following:

`Hello, my name is John and I am 25 years old`

In this example, we used the `Printf` function from the `fmt` package to print a formatted string to the console. We can also use the `Println` function to print a simple string, or the `Print` function to print without a newline. You can find more information and examples on the `fmt` package in the [Go documentation](https://golang.org/pkg/fmt/).

## Deep Dive

The `fmt` package also has a `Debug` function that can be used for more detailed debug output. This function takes in any data type and prints it in a human-readable format. This is useful for debugging complex data structures or variables with multiple values assigned.

```Go
package main

import "fmt"

func main() {
  nums := []int{1, 2, 3}
  fmt.Println(nums)
  fmt.Printf("%#v", nums)
}
```

The first `Println` function will output `[1 2 3]`, while the `Printf` function with `%#v` formatting will output `[]int{1, 2, 3}`. This is much more helpful in understanding the data structure and its values.

Additionally, the `Debug` function can be used with the `go-spew` package to print more complex and nested data structures. This package is not part of the standard library, so it will need to be installed separately using the `go get` command. You can find more information about the `go-spew` package in its [GitHub repository](https://github.com/davecgh/go-spew).

## See Also
- [Go Documentation: Formatting verbs in the `fmt` package](https://golang.org/pkg/fmt/#hdr-Printing)
- [Go Documentation: The `go-spew` package](https://github.com/davecgh/go-spew)