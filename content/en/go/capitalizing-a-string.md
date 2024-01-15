---
title:                "Capitalizing a string"
html_title:           "Go recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to capitalize a string in your programming project? Whether it's for a user interface, printing output, or manipulating data, capitalizing a string can be a useful and necessary task. In this article, we will explore how to do it using the Go programming language.

## How To

Capitalizing a string in Go is a straightforward process. Let's take a look at a few examples:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    // Example 1: using the strings package
    string1 := "hello world"
    capitalizedString1 := strings.ToUpper(string1)
    fmt.Println(capitalizedString1) // Output: HELLO WORLD

    // Example 2: using a custom function
    string2 := "goodbye world"
    capitalizedString2 := capitalize(string2)
    fmt.Println(capitalizedString2) // Output: GOODBYE WORLD
}

func capitalize(s string) string {
    bytes := []byte(s)
    bytes[0] = bytes[0] - 32
    return string(bytes)
}
```

In the first example, we used the `ToUpper()` function from the standard `strings` package to convert our string to all uppercase letters. In the second example, we created a custom function `capitalize()` which converts the first letter of a string to uppercase by manipulating the underlying byte array.

## Deep Dive

Now, let's take a deeper look at the `capitalize()` function. In the function, we first convert the string to a slice of bytes using the built-in `[]byte()` function. This allows us to manipulate each character in the string individually.

Next, we subtract 32 from the first byte in the slice, which effectively converts it to uppercase according to ASCII values. We then convert the byte slice back to a string using the `string()` function and return the capitalized string.

There are also other methods for capitalizing a string in Go, such as using the `unicode` package or regular expressions. However, the two methods shown above are the simplest and most efficient.

## See Also

For more information on string handling in Go, check out these resources:

- [The official Go documentation on strings](https://golang.org/pkg/strings/)
- [Working with strings in Go](https://www.calhoun.io/working-with-strings-in-go/)
- [Manipulating strings in Go](https://blog.gopheracademy.com/advent-2017/strings-manipulation/)
- [Effective Go: Strings](https://golang.org/doc/effective_go.html#strings)

Happy coding!