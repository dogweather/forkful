---
title:                "Using regular expressions"
html_title:           "Go recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions, commonly shortened to "regex," are a powerful tool used in programming to search for patterns within text. They allow programmers to efficiently and accurately manipulate strings of characters, making tasks such as data validation and parsing much easier. Regular expressions have become a staple in programming languages like Go, as they provide a concise and versatile way to handle complex string operations.

## How to:
To use regular expressions in Go, we first import the built-in `regexp` package. This gives us access to functions and methods for working with regular expressions. Let's take a look at a simple example that checks if a string contains the word "Go."

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	str := "I love using Go in my programming projects."
	pattern := "Go"

	match, _ := regexp.MatchString(pattern, str)

	fmt.Println("Match found:", match)
}
```

Running this code will print `Match found: true`, indicating that the string contains the searched pattern. But what if we want to extract the specific location and substring that matched our pattern? We can use `FindStringIndex` and `FindStringSubmatch` methods to achieve this.

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	str := "I love using Go in my programming projects."
	pattern := "Go"

	loc := regexp.MustCompile(pattern).FindStringIndex(str)
	substr := regexp.MustCompile(pattern).FindStringSubmatch(str)

	fmt.Println("Match found at index:", loc)
	fmt.Println("Matched substring:", substr[0])
}
```

The output of this code would be:
```
Match found at index: [19 21]
Matched substring: Go
```

## Deep Dive:
The foundations of regular expressions date back to the 1950s, when computer scientist Stephen Cole Kleene introduced mathematical notation for describing regular languages. This concept was later implemented in various programming languages, and has gone through many iterations and adaptations over the years.

While regular expressions are a popular solution for string manipulation and pattern matching, there are alternatives such as text parsers and string manipulation functions that can also achieve similar results. However, regular expressions have the advantage of being concise and expressive, making them a valuable tool for developers.

Under the hood, regular expressions in Go are implemented using a modified version of the POSIX Extended Regular Expressions (ERE) syntax. This syntax allows for a wide range of pattern matching capabilities, from simple string searches to complex multi-line operations.

## See Also:
- [Go Regexp package documentation](https://golang.org/pkg/regexp/)
- [Regular Expression cheat sheet](https://www.rexegg.com/regex-quickstart.html)