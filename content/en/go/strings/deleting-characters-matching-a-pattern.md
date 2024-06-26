---
date: 2024-02-03 17:50:03.810142-07:00
description: "How to: In Go, deleting characters that match a pattern can be efficiently\
  \ accomplished using the `regexp` package. Here, we'll show how to remove all\u2026"
lastmod: '2024-03-13T22:44:59.617005-06:00'
model: gpt-4-0125-preview
summary: In Go, deleting characters that match a pattern can be efficiently accomplished
  using the `regexp` package.
title: Deleting characters matching a pattern
weight: 5
---

## How to:
In Go, deleting characters that match a pattern can be efficiently accomplished using the `regexp` package. Here, we'll show how to remove all digits, then all non-alphanumeric characters from a string as examples.

1. **Removing All Digits:**

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    text := "Go1 is cool, but Go2 will be cooler! Now: 2023."
	
    // Compile the regular expression for digits
    re, err := regexp.Compile("[0-9]+")
    if err != nil {
        fmt.Println("Error compiling regex:", err)
        return
    }
	
    // Replace digits with an empty string
    result := re.ReplaceAllString(text, "")
	
    fmt.Println(result) // Output: Go is cool, but Go will be cooler! Now: .
}
```

2. **Removing All Non-Alphanumeric Characters:**

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    text := "Go is #1 @ programming languages!"
	
    // Compile the regular expression for non-alphanumeric characters
    re, err := regexp.Compile("[^a-zA-Z0-9]+")
    if err != nil {
        fmt.Println("Error compiling regex:", err)
        return
    }
	
    // Replace non-alphanumeric characters with an empty string
    result := re.ReplaceAllString(text, "")
	
    fmt.Println(result) // Output: Gois1programminglanguages
}
```

## Deep Dive
The `regexp` package in Go provides a powerful interface for pattern matching and manipulation with regular expressions. Its implementation is derived from RE2, a regular expression library designed to guarantee a linear time execution, avoiding the possibility of "catastrophic backtracking" issues present in some other regex engines. This makes Go's regex relatively safe and efficient for a wide array of applications.

While the `regexp` package is a comprehensive solution for dealing with patterns, it's worth noting that for simpler or highly specific string manipulations, other string functions like `strings.Replace()`, `strings.Trim()`, or slicing might offer more performant alternatives. Regular expressions are a powerful tool, but their relative computational expense means that for operations that can be specified without them, exploring standard library alternatives can sometimes lead to simpler and more efficient code.
