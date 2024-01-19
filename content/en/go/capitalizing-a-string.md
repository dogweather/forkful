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

## What & Why?

Capitalizing a string means changing the first letter of a string to its upper-case counterpart. Programmers infrequently use it in formatting text, document titles or creating identifiers.

## How to:

In Go, the `strings` package has a `Title` function which capitalizes the first letter of each word in the string. However, if you only want to capitalize the first letter of the string, you need to manually achieve it. 

```Go
package main

import (
	"fmt"
	"strings"
	    )

func main() {
    // Using Title function
	s := "hello, world"
	fmt.Println(strings.Title(s)) // Outputs: "Hello, World" 

    // Manually capitalizing first letter
	r := []rune(s)
	r[0] = unicode.ToUpper(r[0])
	s2 := string(r)
	fmt.Println(s2) // Outputs: "Hello, world"
}
```
## Deep Dive

Historically, capitalization rules are rooted in typographical practices from the days of letterpress printing. In programming, the concept of capitalizing strings came from the need to properly format text for end users or for proper syntax usage in certain programming languages.

Alternatives to the above methods could include using the `strings.ToUpper` function and then concatenating with the rest of the string. However, this is less efficient than our manual method.

The `strings` package's functions, including `Title`, utilize specific rules of unicode characters to transform them. It uses the global `unicode` package which potentially includes all known upper-case and lower-case pairings.

## See Also:

- Go's official strings library documentation: [https://golang.org/pkg/strings/](https://golang.org/pkg/strings/)
- An useful introduction to Go string manipulation: [https://www.callicoder.com/golang-string/](https://www.callicoder.com/golang-string/)
- Deep-dive into Go's unicode implementation: [https://blog.golang.org/strings](https://blog.golang.org/strings)