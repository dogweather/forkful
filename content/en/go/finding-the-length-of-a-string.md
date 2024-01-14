---
title:                "Go recipe: Finding the length of a string"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why 

Have you ever wondered how programmers are able to determine the length of a string in a programming language? It may seem like a simple task, but there are actually various methods and approaches to accomplish this in Go. In this blog post, we will explore the different ways to find the length of a string in Go, and why it's important in programming.

## How To 

To find the length of a string in Go, we can use the built-in `len()` function. This function takes in a string as its argument and returns the number of characters in that string. Let's see an example:

```
Go is a powerful programming language!
```
`len()` will return `37` as the number of characters in this string, including the white spaces. It's important to note that `len()` counts the number of `bytes` in a string, which may not always be equivalent to the number of `characters` depending on the encoding used.

However, if we want to find the number of runes (Unicode code points) in a string, we can use the `utf8.RuneCountInString()` function. This function also takes in a string as its argument and returns the number of runes. Let's try it out:

```
Go é uma linguagem de programação poderosa!
```

`utf8.RuneCountInString()` will return `38` as the number of runes in this string, which is one more than the number of characters.

## Deep Dive 

In Go, a `string` is in fact a read-only slice of bytes or `[]byte`. This is why we can use the `len()` function to find the length of a string, as slices in Go have a `length` property which gives us the number of elements in the slice. This also explains why `len()` returns the number of bytes, as each character in a string is encoded as a `byte` in Go.

Moreover, Go handles strings and characters differently compared to other programming languages. For example, in Go, a `string` is a series of bytes and not a collection of character values like in Java. This is why we need to use the `utf8.RuneCountInString()` function to find the number of characters or runes in a string.

## See Also
- [Go Strings](https://golang.org/pkg/strings/)
- [UTF-8 and Unicode in Go](https://blog.golang.org/strings)
- [Go Slice Length](https://github.com/golang/example/blob/master/stringutil/stringutil.go#L23)