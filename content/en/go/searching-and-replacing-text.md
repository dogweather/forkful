---
title:                "Searching and replacing text"
html_title:           "Go recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is a common task for programmers, where they look for a specific set of characters or words in a string and replace them with a different set. This can be done manually, but it can be time-consuming and prone to errors, which is why programmers often use coding languages like Go to automate the process.

## How to: 

To search and replace text efficiently in Go, we can use the "strings" package and its built-in functions. Let's take a look at some coding examples:

```Go 
// Import the strings package
import "strings"

// Create a string variable
str := "Hello, world! Hello, Go!"

// Replace "Hello" with "Hey"
newStr := strings.Replace(str, "Hello", "Hey", -1)

// Output: Hey, world! Hey, Go!
fmt.Println(newStr)
```

In the above code, we first imported the "strings" package, which provides functions to manipulate strings. Then, we declared a string variable and used the "Replace" function to replace all instances of "Hello" with "Hey". The last argument (-1) indicates to replace all occurrences, not just the first one. Finally, we printed the new string to see the result.

Now, let's take it a step further and use regular expressions to search and replace text:

```Go 
// Create a string variable 
str := "Buy 2 apples for $3"

// Use regex to replace numbers with "X"
newStr := regexp.MustCompile(`[0-9]+`).ReplaceAllString(str, "X")

// Output: Buy X apples for $X
fmt.Println(newStr)
```

This time, we used the "regexp" package and its "ReplaceAllString" function to replace all numbers with "X" in our string. Regular expressions (regex) are a powerful tool for searching and manipulating text patterns.

## Deep Dive:

Searching and replacing text has been a fundamental task in computing since the early days of programming. In the past, it was often done manually using command-line tools such as grep and sed. However, with the rise of coding languages like Go, this process has become more efficient and less error-prone.

Apart from the string manipulation functions in the "strings" package, Go also has other alternatives for searching and replacing text. For instance, the "bufio" package provides methods to read and write text, and the "bytes" package has functions specifically for byte manipulation in strings.

The "strings" package uses a standard Boyer-Moore implementation to find and replace text efficiently. This algorithm takes advantage of the fact that we are searching for a fixed pattern and can skip large sections of the string that do not match the search pattern.

## See Also:

- [Go strings package documentation](https://golang.org/pkg/strings/)
- [Mastering Regular Expressions by Jeffrey E.F. Friedl](https://www.oreilly.com/library/view/mastering-regular-expressions/9780596528126/)
- [The Go Programming Language by Alan A. A. Donovan and Brian W. Kernighan](https://www.gopl.io/)