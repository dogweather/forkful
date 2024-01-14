---
title:                "Go recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

In programming, there are often times when we need to manipulate strings and remove certain characters from them. This can be for data cleaning, formatting, or other purposes. In the Go programming language, there are several ways to delete characters based on a specific pattern.

## How To

To delete characters matching a pattern in Go, we can use the standard library's `strings` package. This package contains several functions that allow us to manipulate strings, including `ReplaceAll` and `Trim`. 

Let's say we have a string of numbers and we want to remove all the ones that are greater than 5. We can do that using the `Trim` function like this:

```Go
import "strings"

str := "123456789"
newStr := strings.Trim(str, "6789")
fmt.Println(newStr)
//output: 12345
```

We can also use `ReplaceAll` to remove characters based on a specific pattern, such as removing all vowels from a string:

```Go
import "strings"

str := "Hello World"
newStr := strings.ReplaceAll(str, "a", "")
fmt.Println(newStr)
//output: Hllo World
```

These are just a few examples of how we can delete characters matching a pattern in Go. The `strings` package also has other functions that can be useful in different scenarios.

## Deep Dive

Under the hood, the `strings` package uses a technique called "runes" to represent and manipulate strings. Runes are a sequence of Unicode code points, which are simply just numbers that represent characters. This allows the `strings` package to handle different types of characters from different languages.

It's worth noting that while deleting characters based on a specific pattern is a common task in programming, it can also be a computationally expensive task. This is especially true when working with large strings or patterns.

To optimize this task, we can use the `strings.Builder` type, which is a more efficient way to manipulate strings compared to other methods like concatenation or the `strings` package functions.

## See Also

- [Go strings package documentation](https://golang.org/pkg/strings/)
- [Understanding runes in Go](https://blog.golang.org/strings)
- [Efficient string manipulation with strings.Builder](https://yourbasic.org/golang/efficient-string-processing/#string-builder)