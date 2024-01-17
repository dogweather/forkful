---
title:                "Interpolating a string"
html_title:           "Go recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
In simple terms, string interpolation is the process of inserting values or variables into a string, to generate a new string dynamically. Programmers use string interpolation because it allows for convenient and concise string formatting, saving time and effort in creating and managing strings.

## How to:
Go has a built-in string interpolation feature using the fmt package. Here’s an example of how to interpolate a string in Go:
```
Go
package main

import (
	"fmt"
)

func main() {
	name := "John"
	age := 27

	// string interpolation using the fmt package 
	fmt.Printf("My name is %s and I am %d years old.", name, age) 
}
```

Output:
```
My name is John and I am 27 years old.
```

## Deep Dive:
In the past, programmers used concatenation and string formatting methods to generate strings. In Go, there are other ways to interpolate strings such as the “Sprintf” function and string concatenation using the “+” operator. However, both these methods are less efficient and less readable compared to the fmt package interpolation method. The fmt package uses reflection to convert non-string values to strings, resulting in a more efficient and simpler way to interpolate strings.

## See Also:
To further explore string interpolation in Go, check out the official Go documentation on the fmt package: https://golang.org/pkg/fmt/. You can also learn more about string formatting alternatives and their performance comparison on the official Go blog: https://blog.golang.org/go-string-format.