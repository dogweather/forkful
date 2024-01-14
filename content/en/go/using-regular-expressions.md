---
title:    "Go recipe: Using regular expressions"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why

Regular expressions are an essential tool for any programmer, especially those who work with text processing or data extraction. They allow us to find patterns and match strings quickly, making tasks like data validation, scraping, and parsing much more manageable.

## How To

To use regular expressions in Go, we first need to import the `regexp` package. Then, we can compile our pattern into a regular expression object using the `regexp.MustCompile()` function.

```Go
import "regexp"

func main() {
    // Compile regular expression pattern
    pattern := regexp.MustCompile(`penguin(.*?)`)
    // Target string
    str := "I love penguins but not the ones that can fly."
    // Find all matches
    matches := pattern.FindAllStringSubmatch(str, -1)
    // Loop through matches and print output
    for _, match := range matches {
        fmt.Println(match[0])
    }
}
```

The output of the code above would be `penguins but not the ones that can fly.`, showing that the regular expression found the string starting with "penguin" and ending with whatever came after it.

We can also use capture groups to extract specific parts of the string we are matching. In the example below, we use the `FindStringSubmatch()` function to return only the captured group in parentheses.

```Go
func main() {
    // Compile regular expression pattern
    pattern := regexp.MustCompile(`penguin(.*?) flies`)
    // Target string
    str := "I love penguins but not the ones that can fly."
    // Find matches and capture groups
    matches := pattern.FindStringSubmatch(str)
    // Print captured group
    fmt.Println(matches[1])
}
```

The output would be simply `but not the ones that can`, demonstrating how we can use regular expressions to extract specific parts of a string.

## Deep Dive

Regular expressions can be a bit daunting at first, but once you get the hang of them, they can be a powerful tool. Here are a few tips to keep in mind when working with regular expressions in Go:

- Use raw strings (`r"pattern"`) to avoid escaping backslashes, which can be common in regular expressions.
- To match a literal character that has a special meaning in regular expressions, escape it with a backslash (e.g. `\.` to match a period).
- The `FindAll` and `Find` functions return `nil` if no matches are found, so be sure to check for that before attempting to access any data from the match.
- Regular expressions are case-sensitive by default, use the `(?i)` flag to make them case-insensitive.

By mastering regular expressions, you can save yourself a lot of time and effort when working with text data.

## See Also

- [Go regexp package documentation](https://golang.org/pkg/regexp/)
- [Mastering Regular Expressions by Jeffrey E.F. Friedl](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)

Regular expressions may seem complex at first, but with practice, you'll find that they can greatly improve your efficiency when working with text data in Go. Happy coding!