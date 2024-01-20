---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is all about locating specific strings within larger blocks of text and substitifying them with other strings. Programmers perform it to manipulate data, automate text processing, or perform regular cleaning tasks.

## How To:

Here is an example of search and replace action using the `strings.Replace` function in Go.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	fmt.Println(strings.Replace("golang", "go", "fun", -1))
}
```

The output is:

```go
funlang
```

The `strings.Replace` function replaces "go" with "fun" in the string "golang".

## Deep Dive

Historically, Go was developed at Google in 2007 to improve programming productivity and its first version was released in 2012. The `strings.Replace` function has always been a big part of Go's string manipulation toolbox.

There are alternatives to the `strings.Replace` function like the `bytes.Replace` function (where you work with bytes slice rather than strings) or applying regular expressions using the `regexp` package. These alternatives are ideal for more complex search and replace tasks.

The way `strings.Replace` works is interesting! Under the hood, it uses the Two-Way string-matching algorithm. First, it tries to find the substring, and when a match is found, a new string is built by appending the parts.

## See Also

Here are some links to enrich your understanding:

- Official Go documentation: https://golang.org/
- Go string package doc: https://golang.org/pkg/strings/
- Article on Go's Two-Way algorithm: https://levelup.gitconnected.com/the-two-way-string-matching-algorithm-explained-in-golang-da8801c18a2
- Learn about regular expressions in Go: https://gobyexample.com/regular-expressions