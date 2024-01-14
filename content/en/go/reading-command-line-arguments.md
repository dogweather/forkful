---
title:    "Go recipe: Reading command line arguments"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why

Reading command line arguments is an essential skill for any Go programmer. It allows you to pass parameters or options to your program at runtime, making it more versatile and customizable. Plus, understanding how to read command line arguments can greatly improve your overall programming knowledge.

## How To

To read command line arguments in Go, you can use the `os.Args` variable, which is automatically populated with the arguments passed to your program. Let's take a look at a simple example:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    args := os.Args[1:] // exclude the program name itself
    fmt.Printf("Arguments: %v", args)
}
```

If you were to run this program with the command `go run main.go hello world`, the output would be:

```
Arguments: [hello world]
```

As you can see, the `os.Args` variable is a slice of strings, where each element represents an argument passed to the program. By using indexing, you can access individual arguments.

But what if you want to pass options to your program, along with the arguments? For that, we can use flags. The standard library `flag` package makes it easy to define and parse flag options. Let's modify our previous example to include a flag:

```Go
package main

import (
    "flag"
    "fmt"
    "os"
)

func main() {
    // define a flag named "name" with a default value of "World" and short description
    name := flag.String("name", "World", "a name to greet")
    
    // make sure to parse the flags before using them
    flag.Parse()
    
    // get the arguments
    args := os.Args[1:]
    
    // print the greeting with the name and arguments
    fmt.Printf("Hello, %s! Arguments: %v", *name, args)
}
```

Now, if you run `go run main.go -name Alice hello world`, the output would be:

```
Hello, Alice! Arguments: [hello world]
```

Flags can greatly enhance the functionality of your program, so make sure to use them whenever necessary.

## Deep Dive

Reading command line arguments may seem simple, but there are a few important concepts to keep in mind. Firstly, it's worth noting that `os.Args` is a slice that includes the program name as the first element. However, we often want to exclude the program name itself from our arguments. That's why we used the slice expression `[1:]` to exclude the first element in our examples.

Another thing to keep in mind is that arguments are always passed as strings. So if you need to convert them to other types, like integers or booleans, you'll need to use type conversion.

And finally, it's worth mentioning that the `flag` package also allows you to define required flags, set custom usage messages, and even create subcommands. For more details, make sure to check out the links in the "See Also" section below.

## See Also

- [Command Line Arguments in Go](https://gobyexample.com/command-line-arguments)
- [Using Flags in Go](https://golang.org/pkg/flag/)
- [Parsing Command Line Flags in Go](https://blog.rapid7.com/2016/08/04/parsing-command-line-flags-in-go/)