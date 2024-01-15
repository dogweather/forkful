---
title:                "Converting a string to lower case"
html_title:           "Go recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
Converting a string to lower case may seem like a simple task, but it can be incredibly useful in various applications. It allows for easier string comparison and also makes user input more consistent.

## How To
To convert a string to lower case in Go, we can use the strings.ToLower function. Here's an example:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "HELLO WORLD"
	lower := strings.ToLower(str)
	fmt.Println(lower)
}
```

This code will output "hello world" to the console. We first import the "strings" library, which contains the ToLower function. Then, we declare a string variable and assign it a value. Finally, we use the ToLower function to convert the string to lower case and print the result. 

## Deep Dive
It's important to note that the strings.ToLower function only works for ASCII characters. If you are working with non-ASCII characters, you will need to use the unicode.ToLower function instead. This function takes in a rune, which is the equivalent of a character in Go, and returns the lower case rune. 

## See Also
To learn more about string manipulation in Go, check out the official documentation [here.](https://golang.org/pkg/strings/)
You can also explore the unicode package [here.](https://golang.org/pkg/unicode/)