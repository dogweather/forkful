---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern is a basic text processing task in Go. Programmers do it to filter out, replace, or omit certain characters from a string based on specific conditions.

## How to:

Let's dive straight into the code. In Go, we use the `strings.Replace()` or `regexp.ReplaceAllString()` functions. Here’s how you can do it.

For instance, we want to remove all instances of ‘a’ and ‘b’ from a string:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "This is a baba black sheep."
	
	// Deleting all instances of 'a' and 'b'
	result := strings.Replace(strings.Replace(str, "a", "", -1), "b", "", -1)

	fmt.Println(result)
}
```

The output will be:

```
This is   lck sheep.
```

When working with patterns (regex):

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	str := "This is a baba black sheep."
	
	// Pattern to detect 'a' or 'b'
	regex := regexp.MustCompile("[ab]")
	
	// Deleting all matching the pattern
	result := regex.ReplaceAllString(str, "")

	fmt.Println(result)
}
```

The output will be the same:

```
This is   lck sheep.
```

## Deep Dive

Taking a step back, it’s handy to note that character deletion was widely used even in the days of early programming. In Unix, "tr -d" was commonly used to delete characters from a stream or set of files.

Apart from `strings.Replace()` and `regexp` package, Go provides other alternatives such as iterating over the string or using `strings.Builder`. The choice depends on the efficiency you’re after. In fact, for simple replacements `strings.Replace()` is faster, but for pattern matching, `regexp` performs better.

Implementation in Go internally converts the string into a slice of runes. This is because Go's strings are Unicode and can't be dealt with as plain byte arrays. This Unicode-compliant design helps in handling globalized data consistently and universally.

## See Also

- Go documentation on [strings package](https://golang.org/pkg/strings/) and [regexp package](https://golang.org/pkg/regexp/)
- More about [text processing in Go](https://golang.org/pkg/text/)
- [Go Playground](https://play.golang.org/) to tinker with Go code online.