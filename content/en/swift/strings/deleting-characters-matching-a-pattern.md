---
date: 2024-01-20 17:42:56.546141-07:00
description: "How to: Before Swift and modern programming, pattern matching was a\
  \ realm of particular tools and languages like `sed`, `awk`, or Perl known for text\u2026"
lastmod: '2024-04-05T21:53:36.082294-06:00'
model: gpt-4-1106-preview
summary: Before Swift and modern programming, pattern matching was a realm of particular
  tools and languages like `sed`, `awk`, or Perl known for text processing capabilities.
title: Deleting characters matching a pattern
weight: 5
---

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
