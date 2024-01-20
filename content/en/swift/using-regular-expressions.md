---
title:                "Using regular expressions"
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions, or regex, are patterns used to match character combinations in strings. Programmers use them for searching, editing, or validating text, making tasks that deal with string manipulation more efficient and less error-prone.

## How to:
In Swift, you use the `NSRegularExpression` class to handle regex. You define a pattern, create a regex object, and then use it to search or replace text. Here's a basic example:

```Swift
import Foundation

let input = "Call me at 555-1234 or 555-5678."
let pattern = "\\d{3}-\\d{4}" // Matches a pattern like 555-1234

do {
    let regex = try NSRegularExpression(pattern: pattern)
    let matches = regex.matches(in: input, range: NSRange(input.startIndex..., in: input))
    
    for match in matches {
        if let range = Range(match.range, in: input) {
            let phoneNumber = String(input[range])
            print("Found phone number: \(phoneNumber)")
        }
    }
} catch {
    print("Regex error: \(error.localizedDescription)")
}
```

Sample output:
```
Found phone number: 555-1234
Found phone number: 555-5678
```

## Deep Dive
Regex has been around since the 1950s, originating in formal language theory and becoming widely used in Unix tools. In Swift, we use the `NSRegularExpression` class inherited from Objective-C, which relies on the ICU library for Unicode support.

Alternatives to regex in Swift include using `String`'s `contains`, `split`, or `range(of:)` methods for simple cases. For more complex pattern matching, Swift doesn't offer built-in alternatives to regex.

When implementing regex, it's crucial to optimize the pattern to avoid slow searches, especially with large text bodies. Additionally, remember that regex operations can throw exceptions, so always handle them with `try-catch` blocks.

## See Also
- [NSRegularExpression Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Swift String Documentation](https://developer.apple.com/documentation/swift/string)
- [Ray Wenderlich's Guide to NSRegularExpression in Swift](https://www.raywenderlich.com/2725-nsregularexpression-tutorial-and-cheat-sheet)