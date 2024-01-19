---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
In Go, converting a string to lower case means changing all the letters of the string to their lower case equivalences. This is very handy when performing string comparisons, sorting, or when dealing with user inputs to ensure consistency and prevent potential mishaps.

## How to
Go makes it very straightforward to convert a string to lower case with the `ToLower` function in the `strings` package. Here's a quick example:

```Go
package main
import (
    "fmt"
    "strings"
)

func main() {
    str := "Hello, GO!"
    fmt.Println(strings.ToLower(str))
}
```
When you run this code, Go will output:

```
hello, go!
```

## Deep Dive
String case conversion isn't unique to Go; it has been part of programming since the early ASCII days. The logic behind it is pretty simple. Each character actually corresponds to a numeric value. ASCII for example, uppercase 'A' is 65 and lowercase 'a' is 97. To convert between the two, you can simply add or subtract 32.

Alternative ways to do this in Go are using `bytes.Buffer` or looping over the string and manually converting each character, but using `strings.ToLower` is usually more efficient and readable.

The `ToLower` function works by replacing each UTF-8 encoded Unicode code point in the string with its lower case equivalent. It's important to note that this function is case-preserving, which means it leaves any bytes that are not understanding case (like punctuation) untouched.

## See Also
For more info, here are a couple of links that may help:

1. Go's official package documentation: [strings - The Go Programming Language](https://golang.org/pkg/strings/)
2. Great breakdown on how Go strings work: [Strings, bytes, runes and characters in Go - The Go Blog](https://blog.golang.org/strings)