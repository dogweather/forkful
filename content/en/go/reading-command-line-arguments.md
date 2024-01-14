---
title:                "Go recipe: Reading command line arguments"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Reading command line arguments in Go is an essential skill for any developer. It allows for more flexibility and customization in running a program, making it a valuable tool for creating efficient and user-friendly applications.

## How To

To read command line arguments in Go, we will be using the built-in `os` package. First, we need to import it in our code:

```Go
import "os"
```

Next, we can use the `os.Args` variable to access the command line arguments provided to our program. This variable is a slice of strings, with the first element being the path to the program itself.

Let's take a look at a simple example:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Read the command line arguments
    args := os.Args

    // Loop through the arguments and print them
    for i := 0; i < len(args); i++ {
        fmt.Printf("Argument #%d is: %s\n", i, args[i])
    }
}
```

Running this program with `go run arguments.go hello world` would give us the following output:

```
Argument #0 is: /tmp/go-build1635256457/b001/exe/arguments
Argument #1 is: hello
Argument #2 is: world
```

As we can see, the first argument is always the path to our program, while the rest are any arguments provided after it.

We can also access specific arguments by using their index in the `os.Args` slice. For example, `args[1]` would give us the first argument after the program path.

## Deep Dive

Along with `os.Args`, there is also another handy method for reading command line arguments in Go - the `flag` package. This package allows us to define specific flags and arguments for our program, making it easier to handle and parse complex inputs.

For example, we can define a `-name` flag that takes in a string value:

```Go
import (
    "flag"
)

func main() {
    // Define a flag named "name" with a default value of "John"
    name := flag.String("name", "John", "Name to be used in greeting")

    // Parse the flags provided in the command line
    flag.Parse()

    // Print out the greeting using the provided name flag
    fmt.Printf("Hello, %s!\n", *name)
}
```

Running this program with `go run arguments.go -name Alice` would give us the output:

```
Hello, Alice!
```

For more information on using the `flag` package, check out the official documentation [here](https://golang.org/pkg/flag/).

## See Also

- [Go documentation on command line arguments](https://golang.org/pkg/os/#Args)
- [A guide to using flags in Go](https://blog.rapid7.com/2016/08/04/build-a-simple-cli-tool-with-golang/)
- [Handling command line arguments in Go video tutorial by LearnCode.academy](https://www.youtube.com/watch?v=5JAjaTowNDw)