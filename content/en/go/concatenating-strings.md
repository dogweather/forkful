---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings is the process of joining multiple strings together to create a larger string. This is handy when you need to build sentences dynamically or handle pieces of text.

## How To:

Here's a straight-ahead way to concatenate strings in Go:

```Go
str1 := "Hello"
str2 := "World"
str3 := str1 + " " + str2
fmt.Println(str3) // Output: "Hello World"
```

In Go you can also use `fmt.Sprintf`, a way to format strings:

```Go
str1 := "Hello"
str2 := "World"
str3 := fmt.Sprintf("%s %s", str1, str2)
fmt.Println(str3) // Output: "Hello World"
```

Another way is by using the `strings.Join` function for an array of strings:

```Go
strs := []string{"Hello", "World"}
result := strings.Join(strs, " ")
fmt.Println(result) // Output: "Hello World"
```

## Deep Dive:

Historically, + operator is used for concatenating strings in many languages, and Go also preserves this feature. Using + is simple and intuitive, but you should be aware of performance issues when concatenating large amounts of strings or large sized strings.

Go suggests using the strings.Builder or bytes.Buffer which provides an efficient way to concatenate strings.

Here's an example using `strings.Builder`:

```Go
var str strings.Builder

str.WriteString("Hello")
str.WriteString(" ")
str.WriteString("World")

fmt.Println(str.String()) // Output: "Hello World"
```

For `bytes.Buffer` the approach is similar:

```Go
var str bytes.Buffer

str.WriteString("Hello")
str.WriteString(" ")
str.WriteString("World")

fmt.Println(str.String()) // Output: "Hello World"
```

## See Also:

For more details and examples, check the official Go documentation:
- [fmt package](https://golang.org/pkg/fmt/)
- [strings package](https://golang.org/pkg/strings/)
- [bytes package](https://golang.org/pkg/bytes/)

Also, see these articles for a deeper understanding:
1. [String concatenation in Go](https://yourbasic.org/golang/string-concatenation/)
2. [Go by Example: String Functions](https://gobyexample.com/string-functions)

Remember, choosing the correct way to concatenate depends on your scenario. Always keep performance in mind when working with large amounts of data.