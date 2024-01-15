---
title:                "Reading command line arguments"
html_title:           "Go recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Have you ever wondered how command line programs are able to take in input from users? This feature is made possible through the use of command line arguments. Learning how to read command line arguments in Go can enhance your programming skills and make your programs more user-friendly.

## How To

To start reading command line arguments in Go, we first need to import the "os" package:

```Go
import "os"
```

Next, we can use the "Args" function from the "os" package to retrieve command line arguments as a slice of strings:

```Go
args := os.Args
```

To access specific arguments, we can simply use array indexing on the "args" slice:

```Go
arg1 := args[0]
arg2 := args[1]
arg3 := args[2]
```

We can also check the length of the slice to determine how many arguments were provided:

```Go
numArgs := len(args)
```

Let's put it all together and create a program that takes in two command line arguments and prints them out:

```Go
package main

import "fmt"
import "os"

func main() {
    args := os.Args
    if len(args) < 3 {
        fmt.Println("Not enough arguments provided.")
        return
    }
    arg1 := args[1]
    arg2 := args[2]
    fmt.Println("First argument:", arg1)
    fmt.Println("Second argument:", arg2)
}
```

Here's an example of running this program in the command line and providing two arguments:

```
$ go run command_line_args.go hello world
First argument: hello
Second argument: world
```

## Deep Dive

In the above example, we used the "len" function to check the length of the "args" slice. This length includes the name of the program as the first argument. So, if we only wanted to access user-provided arguments, we could use "args[1:]" to create a new slice from the second element to the end.

We can also use the "Flag" package in Go to define and parse command line flags. This is useful for programs that require specific command line options to be provided. Here's an example of how we could use the "Flag" package to create a flag for a user's name:

```Go
package main

import "flag"
import "fmt"

func main() {
    namePtr := flag.String("name", "", "The user's name")
    flag.Parse()

    if *namePtr == "" {
        fmt.Println("Please provide a name with the -name flag.")
    } else {
        fmt.Println("Hello,", *namePtr)
    }
}
```

In the above code, we first create a pointer to a string with the flag name, default value, and description. Then, we call the "Parse" function to assign any provided flag values to the defined pointer. Finally, we can check if a value was provided and use it in our program.

## See Also

- [The "flag" Package in Go](https://golang.org/pkg/flag/)
- [Reading command-line flags in Go](https://peter.bourgon.org/go-in-production/#command-line-flags)