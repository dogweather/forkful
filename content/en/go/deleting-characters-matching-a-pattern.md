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

## Why

Deleting characters matching a pattern is a common task in programming, especially when dealing with user input or data parsing. By learning how to efficiently delete characters, you can streamline your code and make your applications more user-friendly.

## How To

Let's take a look at some examples of how to delete characters matching a pattern using Go.

First, we'll need to import the `regexp` package to work with regular expressions. Regular expressions are sets of characters that define a search pattern and can be used to match and manipulate text.

```Go
import "regexp"
```

To delete characters matching a pattern, we can use the `ReplaceAllString()` function from the `regexp` package. This function takes in three parameters: the original string, the regular expression pattern, and the replacement string. The replacement string will be empty in our case, as we want to delete the characters rather than replacing them.

```Go
s := "Hello, world! How are you?"
pattern := "o"
replacement := ""

result := regexp.ReplaceAllString(s, pattern, replacement)
fmt.Println(result)
// Output: Hll, wrld! Hw are yu?
```

In this example, we used the `ReplaceAllString()` function to delete all instances of the letter "o" in our original string. As you can see, the result string no longer contains the letter "o".

We can also use regular expressions to delete more complex patterns, such as removing all special characters from a string. Let's see how we can achieve that using the `[^a-zA-Z0-9]+` regular expression pattern. This pattern will match any non-alphanumeric characters.

```Go
s := "Hello, world! How are you?"
pattern := "[^a-zA-Z0-9]+"
replacement := ""

result := regexp.ReplaceAllString(s, pattern, replacement)
fmt.Println(result)
// Output: HelloworldHowareyou
```

As you can see, all the special characters in the original string have been deleted, leaving only the alphanumeric characters.

## Deep Dive

Regular expressions can be very powerful and can help us delete characters in a variety of ways. Here are some advanced techniques you can use when working with regular expressions in Go:

- You can use the `FindStringSubmatch()` function to find matches for a given regular expression in a string. This function returns a slice of strings that contain the first match and any captured submatches.
- The `ReplaceAll()` function can also be used to delete characters based on a given pattern. This function takes in a string and a function as parameters, allowing you to perform custom replacements.
- The `ReplaceAllLiteral()` function can be used to replace strings without interpreting any special characters. This can be useful when working with strings that contain special characters such as `$` or `*`.
- Go's `regexp` package also provides the `MustCompile()` function, which compiles a regular expression pattern and returns a `*regexp.Regexp` object. This object can then be used in functions like `ReplaceAllString()` or `FindStringSubmatch()` for increased performance.

## See Also
- [Go Regular Expressions Package Documentation](https://golang.org/pkg/regexp/)
- [Mastering Regular Expressions by Jeffrey E. F. Friedl](https://www.amazon.com/Mastering-Regular-Expressions-Jeffrey-Friedl/dp/0596514271)
- [RegExr - Online Regular Expression Tester](https://regexr.com/)