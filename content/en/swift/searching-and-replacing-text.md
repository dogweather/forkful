---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is a common operation programmers use to change or manipulate strings in various formats. It is crucial for handling data amendment, format correction, and enhancing readability in programming scripts.

## How to:

Here's how you can search and replace a text in Swift:
```Swift
var str = "Hello, World!"
str = str.replacingOccurrences(of: "World", with: "Swift")
print(str) 
//Output will be: "Hello, Swift!"
```

This code searches for the word "World" in the variable `str` and replaces it with "Swift". The `replacingOccurrences()` method does the job here.
 
## Deep Dive 
- **Historical context:** Searching and replacing text has been a key tool in programming since the early days of computing. It originated from text editing software and has been made more straightforward in modern programming languages, including Swift.

- **Alternatives:** There are several ways to search and replace text in Swift. Besides `replacingOccurrences()`, a `range(of: )` method can be used in combination with a `replaceSubrange(:with: )` method.

- **Implementation details:** `replacingOccurrences()` is a method in Swift that replaces all occurrences of the target string with the replacement string within the original string. It is case-sensitive and its execution speed depends on the length of the original string and the number of replacements. 

## See Also 
1. Swift Documentation on strings: [Strings and Characters - Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
2. Apple Developer Documentation: [replacingOccurrences(of:with:options:range:)](https://developer.apple.com/documentation/foundation/nsstring/1416398-replacingoccurrences)
3. Guides on Swift String Manipulation: [Hacking with Swift](https://www.hackingwithswift.com)