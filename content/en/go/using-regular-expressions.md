---
title:    "Go recipe: Using regular expressions"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions, commonly known as regex, are powerful string manipulation tools used in various programming languages. They allow for efficient searching and pattern matching within strings, making it a crucial skill to master for any programmer.

## How To

In Go, regular expressions are supported through the `regexp` package. To start using regex, we first need to import the package:

```Go
import "regexp"
```

Next, we define our regex pattern using the `Compile` method, which takes in a string as its parameter:

```Go
pattern := regexp.MustCompile("go[a-z]+")
```

This pattern searches for any string that starts with "go" followed by one or more lowercase letters. To test our regex pattern, we use the `MatchString` function and pass in the string we want to test against:

```Go
fmt.Println(pattern.MatchString("gopher")) // true
fmt.Println(pattern.MatchString("gOfish")) // false
```

We can also extract specific parts of a string using capturing groups. Let's say we want to extract the username from an email address in the format of "username@example.com". We can use the regex pattern:

```Go
pattern := regexp.MustCompile("(.+)@.+.com")
```

The parentheses indicate the capturing group, and we can use the `FindStringSubmatch` function to retrieve the captured part of the string:

```Go
result := pattern.FindStringSubmatch("john@example.com")
fmt.Println(result[1]) // john
```

Regular expressions offer a wide range of features and syntax, making it a versatile tool for string manipulation. It's important to familiarize yourself with the different functions and patterns available to fully harness its capabilities.

## Deep Dive

Regular expressions in Go also support flags, which add additional functionality to our patterns. Some commonly used flags include:

- `i`: Case-insensitive matching
- `s`: Allow `.` to match newline characters
- `U`: Non-greedy matching

We can include these flags in our regex patterns as a second parameter to the `Compile` function:

```Go
pattern := regexp.MustCompile("go[a-z]+")
```

Regular expressions can also be used for string replacement using the `ReplaceAllString` function. For example, we can replace all occurrences of a substring with a different string:

```Go
pattern := regexp.MustCompile("gopher")
newString := pattern.ReplaceAllString("I love gopher programming", "Go")
fmt.Println(newString) // I love Go programming
```

## See Also

- [Regular Expressions in Go](https://golang.org/pkg/regexp/)
- [Learn Regex Tutorial](https://www.youtube.com/watch?v=rhzKDrUiJVk)
- [Regex Cheat Sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)