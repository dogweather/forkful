---
title:                "Finding the length of a string"
html_title:           "Go recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
If you're new to Go programming, you may be wondering why it's important to know how to find the length of a string. Knowing the length of a string is crucial for many tasks, such as validating user input, manipulating data, and creating efficient algorithms.

## How To
To find the length of a string in Go, you can use the built-in `len` function. This function returns the number of bytes in a given string. For example:

```Go
name := "John"
length := len(name)
fmt.Println(length)
```

This will output `4`, as there are 4 bytes in the string "John". If you need to find the length of a string in a specific encoding, you can use the `utf8.RuneCountInString` function. For example:

```Go
message := "こんにちは"
length := utf8.RuneCountInString(message)
fmt.Println(length)
```

This will output `5`, as there are 5 bytes in the string "こんにちは".

## Deep Dive
Internally, strings in Go are byte arrays, with each character represented by a byte. This is why the `len` function returns the number of bytes in a string. However, this can lead to unexpected results when dealing with multi-byte characters, as shown in the example above. To handle this, Go provides the `utf8.RuneCountInString` function, which counts the number of runes (characters) in a string, taking into account multi-byte characters.

It's worth noting that in Go, strings are immutable, meaning they cannot be changed. This is why you may see functions like `strings.Repeat` or `strings.Replace` return a new string rather than modifying the original. This is also why the `len` function can only return the length of a string and cannot modify it.

## See Also
- [Go Strings Package](https://golang.org/pkg/strings/) - Official documentation on handling strings in Go.
- [Go Tour: Strings](https://tour.golang.org/basics/5) - An interactive tutorial on strings in Go.
- [Master the World of Golang's Strings](https://www.calhoun.io/mastering-the-world-of-golang-strings/) - A comprehensive guide to working with strings in Go.