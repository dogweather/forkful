---
title:                "Go recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

In any programming language, there are times when you need to make changes to a large piece of text or code. For the Go language, a powerful tool for this task is the `strings` package, which allows for efficient searching and replacing of text.

## How To

Using the `strings` package is quite straightforward. First, we import the package into our project:

```Go
import "strings"
```

Next, we declare a string variable that contains the text we want to search and replace:

```Go
text := "Today is a beautiful day, isn't it?"
```

To search for a specific word or phrase, we can use the `Contains()` function:

```Go
strings.Contains(text, "beautiful") // returns true
```

To replace a word or phrase, we can use the `Replace()` function:

```Go
newText := strings.Replace(text, "beautiful", "wonderful", 1) // replaces the first instance of "beautiful" with "wonderful"
```

We can also use the `ReplaceAll()` function to replace all instances of a word or phrase:

```Go
newText := strings.ReplaceAll(text, "day", "night") // replaces all instances of "day" with "night"
```

## Deep Dive

The `strings` package also provides several other useful functions for searching and replacing text. For example, the `Index()` function can be used to find the index of a specific word or phrase within a string, and the `Split()` function can split a string into a slice based on a delimiter.

One important thing to note is that the `strings` package is case-sensitive. This means that in order to accurately replace a word or phrase, the casing must match exactly. To avoid case sensitivity, we can convert the text to lowercase using the `ToLower()` function:

```Go
strings.Replace(strings.ToLower(text), "beautiful", "wonderful", 1) // replaces the first instance of "beautiful" regardless of casing
```

## See Also

- [Official `strings` package documentation](https://golang.org/pkg/strings/)
- [Golang Tutorial: Find and Replace Text in a File](https://golangbot.com/read-write-files/)
- [Mastering Go: Search and Replace Text in a String](https://www.youtube.com/watch?v=026_gyJdldE)