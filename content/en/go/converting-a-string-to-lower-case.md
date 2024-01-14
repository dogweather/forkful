---
title:    "Go recipe: Converting a string to lower case"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why

Converting strings to lower case is a common task in many programming languages. It allows for consistency and easier string comparisons, making it an important function in many applications. In this post, we will explore how to convert strings to lower case in Go.

## How To

To convert a string to lower case in Go, we can use the `strings.ToLower()` function. Let's see a simple example:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	myString := "HELLO WORLD"
	fmt.Println(strings.ToLower(myString))
}
```

In this example, we import the `strings` package which provides a variety of string manipulation functions in Go. Then, we create a string variable `myString` with the value "HELLO WORLD". Finally, we use the `ToLower()` function on the `myString` variable and print the result, which is "hello world".

We can also use this function on a single character or a slice of a string, as shown below:

```Go
fmt.Println(strings.ToLower("G")) // prints g
fmt.Println(strings.ToLower("GoLang")) // prints golang
```

## Deep Dive

The `ToLower()` function in Go uses the Unicode LowerCase mapping, which means it will convert any non-letter runes to their lower case representation. This is useful when dealing with non-English characters or symbols.

Another thing to note is that the `ToLower()` function is case-insensitive, meaning it will convert both upper and lower case letters to their lower case equivalent. This allows for consistent string comparisons, regardless of the original case.

## See Also

- [The Go Programming Language Specification - Strings](https://golang.org/ref/spec#String_types)
- [Go By Example - String Functions](https://gobyexample.com/string-functions)
- [Go Lang Org - String Functions](https://golang.org/pkg/strings/#ToLower)

Converting strings to lower case may seem like a simple task, but it is an important function for maintaining consistency and making string comparisons easier. With the `ToLower()` function in Go, we have a simple and effective way to achieve this. I hope this post has helped you understand and use this function in your own Go programs.