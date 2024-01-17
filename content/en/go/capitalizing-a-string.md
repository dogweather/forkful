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

# Capitalizing Strings in Go

## What & Why?

Capitalizing a string is the process of converting the first character of a string to uppercase. This is a common programming task used in string manipulation and formatting. Programmers do this to ensure consistency in the appearance of text, such as for user input or displaying data in a specific format.

## How to:

In Go, you can capitalize a string by using the `strings` package and its `ToUpper` function. Here is an example of capitalizing the first character of the string "hello":

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	s := "hello"
	fmt.Println(strings.ToUpper(s[0:1]) + s[1:])
}
```
This will output: `Hello`

You can also capitalize the entire string by using the `Title` function instead:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	s := "hello world"
	fmt.Println(strings.Title(s))
}
```
This will output: `Hello World`

## Deep Dive

Capitalizing strings is not a new concept and has been used in programming languages for a long time. It is a way to easily manipulate text and make it more readable. In Go, the `strings` package provides several functions for string manipulation, including the `ToUpper` and `Title` functions.

An alternative to using `strings.ToUpper` for capitalization is the `unicode` package, specifically the `unicode.ToTitle` function. This function not only capitalizes the first character, but also any subsequent characters that have a special meaning in their respective languages. This is relevant for internationalization and localization of software.

When passing an entire string to `strings.ToUpper` or `strings.Title`, it is important to keep in mind that these functions return a new string and do not modify the original string. This is in line with Go's philosophy of immutability, which helps avoid unexpected changes to data.

## See Also

- [strings package documentation](https://golang.org/pkg/strings/)
- [unicode package documentation](https://golang.org/pkg/unicode/)