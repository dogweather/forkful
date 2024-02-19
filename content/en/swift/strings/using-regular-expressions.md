---
aliases:
- /en/swift/using-regular-expressions/
date: 2024-02-03 19:03:07.240000-07:00
description: "Regular expressions, or regex, are sequences of characters that form\
  \ a search pattern, often used for string matching or manipulation tasks. Programmers\u2026"
lastmod: 2024-02-18 23:09:11.386878
model: gpt-4-0125-preview
summary: "Regular expressions, or regex, are sequences of characters that form a search\
  \ pattern, often used for string matching or manipulation tasks. Programmers\u2026"
title: Using regular expressions
---

{{< edit_this_page >}}

## What & Why?
Regular expressions, or regex, are sequences of characters that form a search pattern, often used for string matching or manipulation tasks. Programmers utilize them for everything from data validation and parsing to transformations, making them an indispensable tool in text processing and manipulation tasks across various programming languages, including Swift.

## How to:
Swift native support for regex utilizes the `NSRegularExpression` class, alongside the String class's range and replacement methods. Below is an example of using regex to find and highlight email addresses within a text block:

```swift
import Foundation

let text = "Contact us at support@example.com or feedback@example.org for more information."
let regexPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

do {
    let regex = try NSRegularExpression(pattern: regexPattern)
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))

    if !matches.isEmpty {
        for match in matches {
            let range = Range(match.range, in: text)!
            print("Found: \(text[range])")
        }
    } else {
        print("No matches found.")
    }
} catch {
    print("Regex error: \(error.localizedDescription)")
}

// Sample Output:
// Found: support@example.com
// Found: feedback@example.org
```

For more complex or convenience-focused scenarios, you can use third-party libraries such as SwiftRegex, which simplifies syntax and expands possibilities. Though Swift's standard library is powerful, some developers favor these libraries for their concise syntax and additional features. Here's how you might perform a similar task using a hypothetical third-party library:

```swift
// Assuming a library called SwiftRegex exists and is imported
let text = "Reach out at hello@world.com or visit our website."
let emailPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let emails = text.matches(for: emailPattern) // Hypothetical method provided by SwiftRegex
if emails.isEmpty {
    print("No email addresses found.")
} else {
    emails.forEach { email in
        print("Found: \(email)")
    }
}

// Hypothetical output assuming the `matches(for:)` method exists in SwiftRegex:
// Found: hello@world.com
```

This example illustrates using a third-party regular expression package to simplify finding matches within a string, assuming such convenience methods like `matches(for:)` exist. It's important to refer to the respective third-party library documentation for accurate syntax and method availability.
