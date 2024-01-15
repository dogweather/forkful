---
title:                "Concatenating strings"
html_title:           "Go recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

String concatenation is a commonly used operation in programming, especially when dealing with user input or data from external sources. It allows developers to combine multiple strings into one, creating a cohesive and organized output.

## How To

To concatenate strings in Go, we can use the `+` operator or the `fmt.Sprintf()` function. Let's take a look at some coding examples and their output.

### Using `+` operator

```Go
name := "John"
greeting := "Hello " + name
fmt.Println(greeting)
```

Output:
`Hello John`

### Using `fmt.Sprintf()`

```Go
num1 := 10
num2 := 20
result := fmt.Sprintf("The sum of %d and %d is %d.", num1, num2, num1+num2)
fmt.Println(result)
```

Output:
`The sum of 10 and 20 is 30.`

## Deep Dive

There are a few things to keep in mind when concatenating strings in Go. First, it is important to note that the `+` operator is not just meant for string concatenation, it can also be used for arithmetic operations. Therefore, it is recommended to use `fmt.Sprintf()` when concatenating strings to avoid any unexpected results.

Additionally, Go provides the `strings.Join()` function for more efficient string concatenation. It takes in a slice of strings and a separator, and returns a single string with all the elements of the slice joined together.

```Go
names := []string{"John", "Jane", "Bob"}
result := strings.Join(names, " - ")
fmt.Println(result)
```

Output:
`John - Jane - Bob`

Lastly, it is important to note that string concatenation in Go is not just limited to combining two strings. It can also be used to combine variables, constants, and even expressions.

See Also
- [Official Go Documentation on String Operations](https://golang.org/pkg/strings/)
- [Go By Example: String Formatting](https://gobyexample.com/string-formatting)
- [Go Language Specification: Operators](https://golang.org/ref/spec#Operators)