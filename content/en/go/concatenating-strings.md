---
title:                "Go recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
String concatenation is a common task in programming, especially when dealing with user input or generating dynamic output. It allows us to combine multiple strings into one, making our code more concise and efficient. In this blog post, we will explore how to concatenate strings in Go and delve into some deeper info about this useful operation.

## How To
In Go, the `+` operator is used for string concatenation. Let's see an example of how to concatenate two strings:

```Go
package main

import "fmt"

func main() {
    str1 := "Hello"
    str2 := "world"

    fmt.Println(str1 + " " + str2)
}
```

The output of this code will be: `Hello world`. As you can see, the `+` operator combines the two strings into one, with a space in between.

We can also concatenate strings with variables and other data types. For example:

```Go
package main

import "fmt"

func main() {
    name := "Jane"
    age := 30

    fmt.Println("Hi, my name is " + name + " and I am " + string(age) + " years old.")
}
```

The output of this code will be: `Hi, my name is Jane and I am 30 years old.` Here, we have used the `+` operator to concatenate strings with a variable and an integer converted to a string using the `string()` function.

There is another way to concatenate strings in Go using the `fmt.Sprintf()` function. It takes in a format string and any number of arguments, and returns a formatted string. Let's see an example:

```Go
package main

import "fmt"

func main() {
    name := "John"
    balance := 100.50

    fmt.Println(fmt.Sprintf("Hi %s, your current balance is $%.2f.", name, balance))
}
```

The output of this code will be: `Hi John, your current balance is $100.50.` As you can see, the `fmt.Sprintf()` function allows us to format and concatenate strings in one step.

## Deep Dive
In Go, strings are immutable, which means they cannot be changed. This means that every time we concatenate strings, a new string is created in memory. To avoid unnecessary memory allocation, it is recommended to use the `strings.Builder` type when concatenating multiple strings in a loop.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    var sb strings.Builder

    for i := 0; i < 5; i++ {
        sb.WriteString("Number ")
        sb.WriteString(string(i))
    }

    fmt.Println(sb.String())
}
```

The output of this code will be: `Number 01234`. The `strings.Builder` type allows us to efficiently build and concatenate strings without creating new strings in memory each time.

## See Also
- [Go strings package documentation](https://golang.org/pkg/strings/)
- [Go fmt package documentation](https://golang.org/pkg/fmt/)
- [Example code for string concatenation in Go](https://play.golang.org/p/gxRhp5U0y3O)