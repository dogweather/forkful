---
title:                "Using regular expressions"
html_title:           "Go recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

So, you've heard about regular expressions but you're not exactly sure what they are or why you should use them? Well, let me break it down for you. Regular expressions, also known as regex, are a powerful tool used for string pattern matching and manipulation in programming languages. They allow you to search, replace and extract specific parts of a string, making tasks such as data validation, text parsing and data extraction much easier.

## How To

Using regular expressions in Go is fairly straightforward. First, we need to import the "regexp" package which contains all the necessary functions for working with regex. Now, let's see some examples of how to use regular expressions in Go.

### Matching a String Pattern

To check if a string matches a specific pattern, we can use the `MatchString()` function. For example, if we want to check if a string contains only numbers, we can use the regular expression `\d+` which matches one or more digits.

```
// import regexp package
import "regexp"

func main() {
    // check if string contains only numbers
    matched := regexp.MatchString(`\d+`, "12345")

    if matched {
        fmt.Println("String contains only numbers")
    } else {
        fmt.Println("String contains other characters")
    }
}
```

The output of this code will be `String contains only numbers`.

### Extracting Substrings

Using regex, we can also extract specific parts of a string. For example, if we have a string that follows the format `Name: Age` and we want to extract the name, we can use the `FindStringSubmatch()` function.

```
// import regexp package
import "regexp"

func main() {
    // extract name from string
    name := regexp.MustCompile(`Name: (.+)`).FindStringSubmatch("Name: John")[1]

    fmt.Println("Name:", name)
}
```

The output of this code will be `Name: John`.

## Deep Dive

Regular expressions can seem intimidating at first, but once you understand the basics, they can greatly improve your coding skills. Here are some key points to keep in mind when working with regex in Go:

- Use raw strings, denoted by backticks, when working with regular expressions in Go. This will prevent any issues with escape characters.
- Go's built-in `regexp` package uses the POSIX standard for regular expressions, which may differ from the syntax used in other programming languages.
- Regular expressions can greatly reduce the amount of code needed for tasks such as input validation and text manipulation.

## See Also

- [Official Go regexp package documentation](https://golang.org/pkg/regexp/)
- [Regex tutorial on DigitalOcean](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-go)
- [Online regex tester for Go](https://regex-golang.appspot.com/)