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

## What & Why?

Concatenating strings in programming means combining two or more strings into one. This is a common practice among programmers as it allows for the creation of dynamic and customized output by joining different pieces of text together.

## How to:

To concatenate strings in Go, there are a few simple steps to follow. First, declare the strings you want to combine using the `var` keyword. Then, use the `+` operator to join the strings together in the desired order. Let's see an example:

```
Go func main() {
    name := "John"
    greeting := "Hello"
    message := greeting + " " + name
    fmt.Println(message) // Output: Hello John
}
```

In this code, the string variables `name` and `greeting` are combined with the `+` operator to create the string variable `message`, which is then printed to the console.

## Deep Dive:

Concatenating strings has been a fundamental operation in programming languages for a long time. However, in some languages such as Java, concatenating strings can be inefficient as it creates a new string every time it is performed. In contrast, Go's string concatenation operator `+` is optimized to use strings efficiently and avoid this performance issue.

An alternative to using the `+` operator is the `fmt.Sprintf()` function, which uses placeholders to format and combine strings. This can be useful for more complex string concatenation operations.

Internally, concatenating strings in Go is implemented using a byte array, which ensures efficient memory usage. To avoid creating unnecessary copies of strings, the Go compiler optimizes string concatenation operations at compile time.

## See Also:

- [The official Go documentation on strings](https://golang.org/pkg/strings/)
- [An in-depth explanation of string concatenation in Go](https://www.ardanlabs.com/blog/2017/05/language-mechanics-on-strings.html)