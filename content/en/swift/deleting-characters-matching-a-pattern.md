---
title:                "Deleting characters matching a pattern"
date:                  2024-01-20T17:42:56.546141-07:00
model:                 gpt-4-1106-preview
simple_title:         "Deleting characters matching a pattern"

category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern means removing specific sequences of characters from a string based on a defined pattern, like digits or punctuation. Programmers do it to sanitize inputs, clean data, or prep for processing where specific patterns aren't needed.

## How to:

```swift
import Foundation

// Example: Removing all digits from a String
let originalString = "Contact me at 123-456-7890 after 09:00 PM."
let digitsPattern = "[0-9]"
let resultString = originalString.replacingOccurrences(of: digitsPattern, with: "", options: .regularExpression)

print(resultString)  // Output: "Contact me at -- after : PM."
```

```swift
// Example: Stripping non-alphanumeric characters
let messyString = "H3!llo, W%@rld-"
let nonAlphanumericPattern = "[^A-Za-z0-9]"
let cleanString = messyString.replacingOccurrences(of: nonAlphanumericPattern, with: "", options: .regularExpression)

print(cleanString)  // Output: "H3lloWrld"
```

## Deep Dive

Before Swift and modern programming, pattern matching was a realm of particular tools and languages like `sed`, `awk`, or Perl known for text processing capabilities. Swift, with its robust Foundation framework, simplifies these tasks within the language, making it more accessible to developers.

One alternative to regular expressions is iterating through the string using Swift’s `filter` method coupled with a custom condition, which can also be time-consuming and less readable. Regular expressions offer a compact, albeit sometimes cryptic, way of describing the pattern we want to remove or manipulate.

Under the hood, when you run `replacingOccurrences(of:with:options:)` with `.regularExpression` option, Swift uses ICU's (International Components for Unicode) regular expression engine to process the pattern. ICU is a mature, widely-used library for Unicode support, including pattern matching, that's built into many high-level programming languages.

## See Also

- Swift String Documentation: https://developer.apple.com/documentation/swift/string
- Swift Regular Expressions: https://developer.apple.com/documentation/foundation/nsregularexpression
- ICU User Guide for Regular Expressions: https://unicode-org.github.io/icu/userguide/strings/regexp.html
