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

## Why

Have you ever needed to extract a specific part of a string in your Go code? Maybe you need to retrieve part of a URL or format a user's input. Whatever the reason may be, extracting substrings can be a useful skill to have in your programming arsenal.

## How To

To extract a substring in Go, we can use the `Substr` function from the `Strings` package. This function takes in the original string, the starting index, and the desired length of the substring. For example:

```Go
str := "Hello World"
substring := strings.Substr(str, 6, 5)
fmt.Println(substring) // Outputs "World"
```

In the above code, we start at index 6 (which is the letter "W") and extract a substring of length 5, resulting in "World" being printed to the console.

Another way to extract substrings in Go is by using the `Slice` function from the `strings` package. This function takes in the original string and a range of indices to determine the substring. For example:

```Go
str := "Hello World"
substring := str(6:11)
fmt.Println(substring) // Outputs "World"
```

In this code, we specify a range of indices starting at index 6 and ending at index 11 (excluding the element at index 11), resulting in the same output as the previous example.

## Deep Dive

It's important to note that Go strings are immutable, meaning that they cannot be modified directly. Therefore, when we extract a substring, a new string is created and returned. This means that if we attempt to modify the extracted substring, it will not affect the original string.

Another useful function to use when extracting substrings is the `Index` function from the `strings` package. This function takes in a string and a substring and returns the index of the first occurrence of the substring in the string. This can be helpful when trying to get the starting index for the `Substr` function.

## See Also

- [Go Strings package documentation](https://golang.org/pkg/strings/)
- [Official Go language website](https://golang.org/)