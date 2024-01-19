---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string is the process of determining how many characters (including spaces and punctuation) a string contains. Programmers use it for various reasons such as validating inputs, slicing strings, or checking for emptiness.

## How to:

Here's a quick way to get the string length in Go:

```Go
package main

import "fmt"

func main() {
    str := "Hello, world!"
    fmt.Println(len(str)) // Outputs: 13
}
```

In this example, `len` is a built-in Go function, and `str` is our string variable.

## Deep Dive

Historically, getting the length of a string involved creating a loop and counting characters one by one until hitting the null character which signifies the end of the string. But, Go makes it simpler with its `len` function.

An alternative way in Go, especially for multi-byte (UTF-8) strings, is using the `utf8.RuneCountInString` function:

```Go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	str := "Привет, мир!"
	fmt.Println(utf8.RuneCountInString(str)) // Outputs: 12
}
```
This will give accurate results by counting runes (Unicode points) not bytes.

Behind the scenes, Go stores strings as a byte array. Hence, the time complexity of the `len` function is constant, i.e., O(1). Beware, different Unicode characters might take up varying number of bytes.

## See Also

1. Official Go docs on strings: https://golang.org/pkg/strings/
2. Go blog post on Strings, bytes, runes and characters: https://blog.golang.org/strings
3. More about Unicode in Go: https://www.alexedwards.net/blog/understanding-unicode