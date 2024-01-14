---
title:                "Go recipe: Searching and replacing text"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Searching and replacing text is a common task in programming, especially when working with large amounts of text data. It allows for efficient and automated changes to be made in a text file or codebase. Go language offers powerful tools and methods for text manipulation, making it a popular choice for tasks such as these.

## How To

To perform a search and replace operation in Go, we can use the `strings.Replace()` function. It takes in three string parameters - the input string, the old substring to be replaced, and the new substring. Let's see an example:

```
package main

import (
	"fmt"
	"strings"
)

func main() {
	input := "Hello World"
	result := strings.Replace(input, "World", "Universe", 1)
	fmt.Println(result)
}
```

The output of this code will be `Hello Universe`, as expected. The fourth parameter in `strings.Replace()` allows you to specify the number of replacements to be made. By default, it replaces all occurrences.

We can also use regular expressions for more complex search and replace scenarios. Go provides a `regexp` package for this purpose. Let's take a look at a code example:

```
package main

import (
	"fmt"
	"regexp"
)

func main() {
	input := "Today is 09/01/2021"

	re := regexp.MustCompile(`(\d{2})/(\d{2})/(\d{4})`)
	result := re.ReplaceAllString(input, "${3}-${2}-${1}")

	fmt.Println(result)
}
```

In this example, we use a regular expression to capture the date in the `MM/DD/YYYY` format and replace it with the `YYYY-MM-DD` format. The output of this code will be `Today is 2021-09-01`.

## Deep Dive

The `strings.Replace()` function internally uses an algorithm called Boyer-Moore-Horspool for efficient and fast string replacement. It also supports case-sensitivity and Unicode characters.

The `regexp` package includes methods such as `FindString()` and `FindAllString()` which allow for more granular control over the search and replace operations. Additionally, it provides support for advanced regular expression features like lookarounds and backreferences.

In more complex scenarios, we can also use the `bufio` package to perform line-by-line search and replace in a text file.

See Also
- [Go Strings Package](https://pkg.go.dev/strings)
- [Go Regexp Package](https://pkg.go.dev/regexp)
- [Go Bufio Package](https://pkg.go.dev/bufio)