---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings is about retrieving a part of a string based on specific positions or patterns. It's a useful tool for things like parsing data or processing text.

## How to:

Here's how you extract substrings in Go. We use the slice syntax to accomplish this.

```Go
package main

import "fmt"

func main() {
   str := "Hello, World!"
   subStr := str[0:5]
   fmt.Println(subStr)
}
```

When you run this code, it gives you:

```
Hello
```

You can also dynamically determine the end value, to select up to the last character of the string:

```Go
package main

import "fmt"

func main() {
   str := "Hello, World!"
   subStr := str[0:len(str)]
   fmt.Println(subStr)
}
```

This gives you the whole string:

```
Hello, World!
```

## Deep Dive

- **History**: Unlike some other languages, Go does not have a separate substring function/method. This characteristic is derived from Go's approach of being minimalist and efficient.

- **Alternatives**: Regex can be used for more complex substring extractions, like when a pattern is involved. For instance, the `regexp` package provides functions to find substrings based on regex patterns.

- **Details**: The slice syntax `str[start:end]` gives a part of the string from index `start` up to (but not including) index `end`. The first character in `str` is at index 0, like in most programming languages. But, be cautious, accessing a string out of its valid range can lead to runtime panic.

## See Also 

Check out these additional resources for string manipulations and regular expressions in Go:

1. [String manipulations in Go](https://golang.org/pkg/strings/)
2. [Regular expressions in Go](https://golang.org/pkg/regexp/)
3. [Go by Example: String Functions](https://gobyexample.com/string-functions)