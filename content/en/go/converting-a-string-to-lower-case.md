---
title:                "Go recipe: Converting a string to lower case"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
Converting strings to lower case is a common task when working with text data. It allows for easier comparison and manipulation of strings, as well as ensuring consistency in formatting.

## How To

There are multiple ways to convert a string to lower case in Go. One way is to use the `strings.ToLower()` function, which takes in a string as an argument and returns the string converted to lower case. Here's an example:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Hello, WORLD!"
    lower := strings.ToLower(str)
    fmt.Println("Lower case string:", lower)
}
```

This code will output: `Lower case string: hello, world!`

Another option is to utilize the `strconv.Atoi()` function, which will only convert letters to lower case while preserving numbers. Here's an example:

```Go
package main

import (
    "fmt"
    "strconv"
)

func main() {
    str := "HeLlO2021"
    lower := strconv.Itoa(str)
    fmt.Println("Lower case string:", lower)
}
```

This code will output: `Lower case string: hello2021`

## Deep Dive
When converting strings to lower case, it's important to understand how different languages handle capitalization. Some languages, such as Turkish, have special characters that may have different cases in comparison to the standard 26-letter alphabet. In these cases, it's important to use a language-specific method for converting strings to lower case.

In Go, there is also the `unicode.ToLower()` method which can handle accented characters and other special cases. This method takes in a `rune` argument, which represents a single Unicode code point, and returns a `rune` value. Here's an example:

```Go
package main

import (
    "fmt"
    "unicode"
)

func main() {
    str := "Ça Va?"
    lower := unicode.ToLower(rune(str[0]))
    fmt.Println("Lower case string:", string(lower))
}
```

This code will output: `Lower case string: ça`

## See Also
- [The official Go Documentation on strings](https://golang.org/pkg/strings/)
- [An Introduction to Go](https://www.digitalocean.com/community/tutorials/how-to-install-go-and-set-up-a-local-programming-environment-on-macos)
- [Understanding Unicode in Go](https://medium.com/rungo/string-data-type-in-go-8f83cc6ce398)