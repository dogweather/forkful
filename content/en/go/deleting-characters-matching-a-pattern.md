---
title:                "Deleting characters matching a pattern"
html_title:           "Go recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Deleting characters that match a certain pattern is a common task in programming. It involves identifying specific characters within a string and removing them based on a defined pattern. Programmers often do this to clean up data or manipulate strings in a more efficient way.

## How to:
To delete characters matching a pattern in Go, we can use the strings.ReplaceAll() function. This function takes in three parameters: the original string, the pattern to be deleted, and the replacement value. Here is an example:

```
// Defining and initializing our original string
originalString := "Hello, Go!"

// Deleting the letter "o" from our string
newString := strings.ReplaceAll(originalString, "o", "")

// Printing the new string
fmt.Println(newString)

// Output: Hell, G!
```

We can also use regular expressions to delete multiple patterns from a string. The regexp package in Go provides useful functions for pattern matching and deletion. Here is an example:

```
// Defining and initializing our original string
originalString := "Go is awesome!"

// Creating a regular expression to match vowels
pattern := regexp.MustCompile("[aeiou]")

// Replacing vowels with empty strings
newString := pattern.ReplaceAllString(originalString, "")

// Printing the new string
fmt.Println(newString)

// Output: G s wsm!
```

## Deep Dive:
The concept of deleting characters matching a pattern has been around since the early days of programming languages. In Go, the strings.ReplaceAll() function was added in version 1.12. The regexp package, which allows for more complex pattern matching, has been a part of Go since its initial release.

There are also alternative methods for deleting characters in Go, such as using the strings.Replace() function which allows for specifying the number of replacements to be made. Another approach is to use the strings.Builder type, which provides more efficient handling of string modifications.

In terms of implementation, the strings.ReplaceAll() function uses a simple string search and replace algorithm, while the regexp package utilizes regular expressions for more advanced matching and replacement.

## See Also:
- [Go documentation on strings package](https://golang.org/pkg/strings/)
- [Go documentation on regexp package](https://golang.org/pkg/regexp/)
- [Tutorial on manipulating strings in Go](https://www.calhoun.io/inserting-and-removing-characters-from-a-string-in-go/)