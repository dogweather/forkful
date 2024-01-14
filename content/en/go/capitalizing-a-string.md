---
title:    "Go recipe: Capitalizing a string"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

One of the most common tasks in programming is manipulating strings. Capitalizing a string is a simple but essential task in many applications, whether it's for formatting text or validating user input. Understanding how to capitalize a string in Go can be useful for any programmer, from beginners to experienced developers.

## How To

To capitalize a string in Go, we can use the `strings` package which provides several functions for manipulating strings. One of these functions is `Title()`, which capitalizes the first letter of each word in a string.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    myString := "hello world"
    result := strings.Title(myString)
    fmt.Println(result)
}
```

The above code will output: `Hello World`. We first declare a variable `myString` with the value of `hello world`. Then, we use the `strings` package and its `Title()` function to capitalize the string and assign it to the variable `result`. Finally, we print out the result.

Another method to capitalize a string in Go is by converting it to a `rune` slice and manually capitalizing the first letter.

```Go
package main

import (
    "fmt"
    "unicode"
)

func main() {
    myString := "hello world"
    runes := []rune(myString)
    runes[0] = unicode.ToUpper(runes[0])
    result := string(runes)
    fmt.Println(result)
}
```

This code will also output: `Hello world`. We first convert the string to a `rune` slice, where each character is a `rune` type. Then, we use the `unicode` package to capitalize the first rune and assign it back to the `rune` slice. Finally, we convert the `rune` slice back to a string and print the result.

## Deep Dive

Under the hood, the `Title()` function in the `strings` package uses the `map` function from the `unicode` package to capitalize each word in a string. This function maps every character in the string to its corresponding title case letter, based on the Unicode character properties.

In the second method, we manually convert a string to a `rune` slice and use the `unicode` package to capitalize the first rune. This is useful for when we only want to capitalize the first letter of a string and not every word.

## See Also

- [The strings Package in Go](https://golang.org/pkg/strings/)
- [The unicode Package in Go](https://golang.org/pkg/unicode/)
- [Learn Go in Y Minutes: Working With Strings](https://learnxinyminutes.com/docs/go/)