---
title:    "Go recipe: Printing debug output"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why
Debugging is an essential aspect of programming, and one of the most common and useful techniques is printing debug output. This allows developers to see the values of variables at a specific point in their code, helping them identify and fix errors more efficiently. In this blog post, we will explore how to print debug output in the Go programming language.

## How To
To print debug output in Go, we can use the `fmt.Println()` function. This function takes in one or more values or variables and prints them to the standard output.

```
package main

import "fmt"

func main() {
    var num1 = 5
    var num2 = 10
    fmt.Println("The value of num1 is:", num1)
    fmt.Println("The value of num2 is:", num2)
}
```

Output:
```
The value of num1 is: 5
The value of num2 is: 10
```

We can also use `fmt.Printf()` to print formatted debug output. This function allows us to specify the format of the output and pass in multiple values.

```
package main

import "fmt"

func main() {
    var str = "Hello"
    var num = 3.14
    fmt.Printf("The value of str is: %s \n", str)
    fmt.Printf("The value of num is: %f \n", num)
}
```

Output:
```
The value of str is: Hello
The value of num is: 3.140000
```

## Deep Dive
Apart from the `fmt` package, we can also use the `log` package for printing debug output in Go. The `log` package provides functions for printing different types of output, including warnings, errors, and info messages. 

```
package main

import (
    "fmt"
    "log"
)

func main() {
    num := 20
    log.Println("This is a log message.")
    log.Printf("The value of num is: %d", num)
}
```

Output:
```
2021/09/13 12:34:56 This is a log message.
2021/09/13 12:34:56 The value of num is: 20
```

We can also customize the logging output by setting different flags. For example, we can add a timestamp to each message by using the `log.LstdFlags` flag.

```
log.SetFlags(log.LstdFlags)
```

Now, the output will include a timestamp along with the message.

## See Also
- [Debugging in Go](https://golang.org/doc/gdb)
- [A Tour of Go: Debugging](https://tour.golang.org/debugging)
- [Advanced Debugging in Go](https://medium.com/a-journey-with-go/go-advanced-debugging-with-delve-7b30a34d9beb)