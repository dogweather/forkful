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

## What & Why?

Finding the length of a string is a common task in programming where we need to determine the number of characters in a given string. This is useful for tasks like validating input or manipulating text data. By knowing the length of a string, we can perform certain operations on it with precision.

## How to:

The Go programming language provides a built-in function called `len()` which we can use to find the length of a string. Here's an example:

```Go
str := "Hello World"
length := len(str)
fmt.Println(length) // Output: 11
```

We can also use this function to find the length of a string that contains non-English characters, like in this example:

```Go
str := "こんにちは" // Hello in Japanese
length := len(str)
fmt.Println(length) // Output: 5
```

## Deep Dive

The concept of finding the length of a string has been around since the early days of programming. In languages like C and C++, the only way to find the length of a string was by iterating over each character and counting them. This was a tedious task, and it was prone to errors.

In contrast, modern languages like Go provide built-in functions like `len()` to make this task simpler. Other languages may use different approaches, such as using a property or method, but the end result is the same - to obtain the length of a string.

## See Also

If you want to dive deeper into the technical details of finding the length of a string, you can check out the [Go documentation](https://golang.org/pkg/builtin/#len). Additionally, there are many online resources that discuss the various ways to find the length of a string in different programming languages.