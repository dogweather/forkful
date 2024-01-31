---
title:                "Using regular expressions"
date:                  2024-01-19
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) are patterns used to match character combinations in strings. Programmers use them for searching, validating, and manipulating text, making them a swiss army knife for string operations.

## How to:
```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    // Example: Finding emails in a string
    text := "Reach out at contact@example.com or support@random.org"
    emailRegex := regexp.MustCompile(`[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}`)

    // FindString returns the first match
    fmt.Printf("First email: %s\n", emailRegex.FindString(text)) 
    // Output: First email: contact@example.com

    // FindAllString returns all matches
    emails := emailRegex.FindAllString(text, -1)
    fmt.Printf("All emails: %v\n", emails) 
    // Output: All emails: [contact@example.com support@random.org]

    // Replacing text
    sanitizedText := emailRegex.ReplaceAllString(text, "[redacted]")
    fmt.Println(sanitizedText) 
    // Output: Reach out at [redacted] or [redacted]
}
```

## Deep Dive
Regex has Unix origins in the 1950s, getting traction through tools like `grep`. Perl later popularized them. Alternatives include using string functions or parsers for simple and structured data, respectively. Implementation-wise, Go's `regexp` package is NFA-based (non-deterministic finite automaton), handling regex efficiently without backtracking pitfalls found in some other engines.

## See Also
- Go `regexp` package documentation: [pkg.go.dev/regexp](https://pkg.go.dev/regexp)
- Online regex tester and debugger: [regex101.com](https://regex101.com/)
- Mozilla Developer Network regex guide: [developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
