---
title:                "Go: सबस्ट्रिंग निकालना"
simple_title:         "सबस्ट्रिंग निकालना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substring extraction is a crucial aspect of string manipulation in any programming language. In Go, it allows us to extract specific parts of a string and use them for various purposes such as data processing, pattern matching, and more. As a Hindi reader, understanding how to extract substrings in Go can enhance your coding skills and make your programs more efficient.

## How To

To extract substrings in Go, we use the `Substring()` function from the `strings` package. This function takes in two parameters - `start` and `end` - which indicate the starting and ending indices of the desired substring. Here's an example code that extracts a substring from a given string:

```Go
str := "Hello, World!"
sub := str[0:5] // extracting substring "Hello"
fmt.Println(sub) // outputs "Hello"
```

We can also use `Substring()` to extract a portion of a string from a specific index until the end, by omitting the `end` parameter. Here's an example:

```Go
str := "Hello, World!"
sub := str[7:] // extracting substring "World!"
fmt.Println(sub) // outputs "World!"
```

We can also use negative indices for both `start` and `end` parameters to indicate a distance from the end of the string. This allows us to extract substrings from the end. Here's an example:

```Go
str := "Hello, World!"
sub := str[-7:] // extracting substring "World!"
fmt.Println(sub) // outputs "World!"
```

## Deep Dive

In Go, strings are immutable and hence, the `Substring()` function does not modify the original string, but instead creates a new string. This means that the values of `start` and `end` indices must be within the bounds of the original string, otherwise, it will result in an error. Additionally, Go also supports the use of runes to handle multi-byte characters, which may affect the results of substring extraction.

## See Also

To learn more about strings and substring extraction in Go, check out the following resources (all links in Hindi):

- [Go Strings Tutorial](https://www.studytonight.com/go/strings-in-go.php)
- [Substring Extraction in Go](https://medium.com/@sarathkumarkb/know-your-strings-in-go-series-3-substrings-3f05e76d351e)
- [Rune Handling in Go](https://cmdrsrvr.com/posts/part-03-go-rune-data-type)
- [Go Documentation on Strings](https://golang.org/pkg/strings/)