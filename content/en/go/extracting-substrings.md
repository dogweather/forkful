---
title:    "Go recipe: Extracting substrings"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why

Substring extraction is a common task in many programming languages, including Go. It involves retrieving a specific portion of a string, rather than the entire string itself. This can be useful for tasks such as parsing data or manipulating text.

## How To

To extract substrings in Go, we can use the built-in `substring` function. This function takes in two parameters: the string we want to extract from, and the starting and ending index of the substring. Let's see an example of how this works:

```Go
myString := "Hello, world!"
substring := myString[0:5]
fmt.Println(substring)
```

The above code will output `Hello`, since we are extracting the first 5 characters of the string.

We can also use negative indices to extract substrings from the end of the string. For example:

```Go
myString := "Hello, world!"
substring := myString[7:]
fmt.Println(substring)
```

This code will output `world!`, since we are extracting everything after the 7th character (including the 7th character).

We can also use the `len` function to determine the length of the string and use it as the ending index. For example:

```Go
myString := "Hello, world!"
length := len(myString)
substring := myString[length-1:]
fmt.Println(substring)
```

This code will output `!`, since we are extracting the last character of the string.

## Deep Dive

The `substring` function in Go is an efficient and flexible way to extract substrings. It is important to note that the ending index in the function is exclusive, meaning it will not include the character at that index in the substring. 

Additionally, we can also use the `strings` package in Go to perform more complex substring operations, such as searching for a specific substring or replacing substrings. This can come in handy for tasks such as data cleaning and manipulation.

## See Also

- [Official Go documentation for the `substring` function](https://golang.org/pkg/strings/#example_Substring)
- [Tutorial on extracting substrings in Go](https://www.callicoder.com/golang-strings-cheat-sheet/#substring)
- [String manipulation in Go](https://www.golangprograms.com/go-language/strings.html)