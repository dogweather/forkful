---
title:                "Removing quotes from a string"
date:                  2024-01-25T20:50:06.222789-07:00
model:                 gpt-4-1106-preview
simple_title:         "Removing quotes from a string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Stripping quotes from a string means getting rid of those pesky double or single quote characters wrapping your actual text. We do this to sanitize data, prevent parsing errors, or prep text for further processing without the added fluff of quotation marks.

## How to:

Here's the simple way to kick those quotes to the curb in Go:

```go
package main

import (
	"fmt"
	"strings"
)

func removeQuotes(s string) string {
	return strings.Trim(s, "'\"")
}

func main() {
	quotedString := "\"Hello, World!\""
	fmt.Println("Original:", quotedString)

	unquotedString := removeQuotes(quotedString)
	fmt.Println("Unquoted:", unquotedString)
}
```

Output will look like this, quotes all gone:

```
Original: "Hello, World!"
Unquoted: Hello, World!
```

## Deep Dive

Back in the day, when data formats and interchange weren't standardized, quotes in strings could cause havoc. They still can, especially in JSON or when shoving strings into databases. The `strings` package in Go comes loaded with a `Trim` function, which nixes not just whitespace but any characters you're not a fan of.

Why not Regex? Well, `Trim` is faster for simple jobs, but if your strings are playing hide and seek with quotes in weird places, regex might be your heavy artillery:

```go
import "regexp"

func removeQuotesWithRegex(s string) string {
	re := regexp.MustCompile(`^["']|["']$`)
	return re.ReplaceAllString(s, "")
}
```

It's like choosing between scissors and a chainsaw; pick the tool fit for the job.

## See Also

For more on the `strings` package and its power tools:
- [Package strings](https://pkg.go.dev/strings)

To wield the might of regular expressions in Go:
- [Package regexp](https://pkg.go.dev/regexp)

Want to dive into the philosophy of string trimming?
- [The Trim Method](https://blog.golang.org/strings)
