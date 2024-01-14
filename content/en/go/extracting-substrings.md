---
title:                "Go recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Extracting substrings is a useful skill to have for any Go programmer. By taking a portion of a larger string, it allows for more efficient processing and manipulation of data.

## How To

Let's dive into some coding examples on how to extract substrings in Go.

```Go
//Example 1: Substring starting from a given index
str := "Hello World!"
substring := str[6:] // substring = "World!"
```

In this example, we use the slice operator `[startIndex:endIndex]` to specify the range of characters we want to extract. By omitting the endIndex, it defaults to the end of the string, allowing us to extract all characters from the given index to the end.

```Go
//Example 2: Substring of a specific length
str := "Hello World!"
substring := str[1:5] // substring = "ello"
```

Here, we use the same slice operator to specify the start and end index, but this time we limit the length of the substring by specifying an endIndex. This allows us to extract a specific portion of a string.

```Go
//Example 3: Substring with a custom function
func substring(str string, start int, length int) string {
    runes := []rune(str)
    return string(runes[start:start+length])
}

str := "Hello World!"
substring := substring(str, 1, 5) // substring = "ello"
```

This example uses a custom function, `substring`, to extract a substring from a given string. By converting the string to a slice of runes, we can use the index values to specify the start and end of the substring.

## Deep Dive

In Go, strings are immutable, meaning they cannot be changed once created. This presents a hurdle when trying to extract substrings, as a new string must be created to hold the extracted characters.

To overcome this, Go's `strings` package offers the `Substring()` function that simplifies extracting substrings. Additionally, some important things to keep in mind when extracting substrings are:

- The index values should be within the length of the string, otherwise it will result in a "slice bounds out of range" error.
- Negative index values can be used to extract substrings starting from the end of the string.
- The endIndex value can be greater than the length of the string, in which case it will default to the end of the string.
- Unicode characters can be extracted using the `strings.Index()` function to find their index value.

## See Also

- Official documentation on strings package: https://golang.org/pkg/strings/
- A beginner's guide to Go: https://golangbot.com/learn-golang-series/
- More examples of string manipulation in Go: https://www.callicoder.com/golang-strings-guide/