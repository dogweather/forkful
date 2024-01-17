---
title:                "Extracting substrings"
html_title:           "Go recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extracting substrings is the process of extracting a portion of a string based on a certain starting and ending index. Programmers often do this when they need to manipulate or analyze smaller sections of a larger string, such as grabbing a specific date from a longer text or separating a name into first and last names.

## How to:
To extract a substring in Go, you can use the `Substr()` function from the `strings` package. The function takes in three parameters: the original string, the starting index, and the ending index (exclusive). Here's an example of extracting a substring from the string "Hello World" at indices 3 and 5:
```Go
sub := strings.Substr("Hello World", 3, 5)
fmt.Println(sub)
```
The output should be "lo".

## Deep Dive:
Historically, extracting substrings has been an important function in programming to manipulate strings. In the past, it was commonly used to find and manipulate specific characters or words within a large block of text. In modern programming, there are alternative methods for manipulating strings, such as regular expressions. However, extracting substrings is still a useful tool for certain tasks. 

In Go specifically, the `Substr()` function uses a zero-based indexing system, meaning the first character in a string is at index 0. The ending index is exclusive, meaning it will not include the character at that index. For example, using an end index of 5 would include characters up to index 4 but not including 5. 

## See Also:
- [Go Strings Package Documentation](https://golang.org/pkg/strings/#Substring)
- [Regular Expressions in Go](https://golang.org/pkg/regexp/)