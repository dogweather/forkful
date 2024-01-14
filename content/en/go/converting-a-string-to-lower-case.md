---
title:                "Go recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##Why

Converting strings to lower case may seem like a small task, but it can make a big difference in the functionality and user experience of a Go program. It allows for better handling of user input and ensures consistency in string comparison.

##How To

To convert a string to lower case in Go, we can use the `strings.ToLower()` function. Let's take a look at an example:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Go PROGRAMMING"
    lower := strings.ToLower(str)
    fmt.Println(lower)
}
```

The output of this program will be `go programming`. We can also use this function on user input to ensure that the program is able to handle variations in capitalization.

##Deep Dive

Behind the scenes, the `strings.ToLower()` function uses the package `unicode` to handle different character sets, making it a reliable and efficient method for converting strings to lower case. It also takes into account any special cases, such as accents and diacritics.

It's important to note that in Go, strings are immutable, meaning that the original string does not change when we use `strings.ToLower()`. Instead, a new string is created and returned. This helps to avoid any unintended changes to the original input string.

##See Also

To learn more about working with strings in Go, check out these resources:

- [Go Documentation on strings](https://golang.org/pkg/strings/)
- [A Tour of Go - Strings](https://tour.golang.org/basics/3)
- [Gophercises - Writing a CLI Task Manager in Go](https://gophercises.com/exercises/task)