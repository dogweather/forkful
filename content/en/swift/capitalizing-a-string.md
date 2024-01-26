---
title:                "Capitalizing a string"
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalization means changing the first letter of words to uppercase; in strings, it's often about formatting or making text user-friendly. Programmers capitalize strings for readability, to follow grammatical rules, or to match a style guide.

## How to:
Swift makes capitalizing strings straightforward. Here's a quick tour:

```Swift
let lowercasedString = "hello, world!"
let titleCased = lowercasedString.capitalized // "Hello, World!"
let uppercasedString = lowercasedString.uppercased() // "HELLO, WORLD!"

// Sample Output:
print(titleCased)  // Prints "Hello, World!"
print(uppercasedString)  // Prints "HELLO, WORLD!"
```

For more control, we'll dabble with the `Locale`:

```Swift
let sentence = "the quick brown fox"
let titleCasedWithLocale = sentence.capitalized(with: Locale(identifier: "en_US"))
// "The Quick Brown Fox"

// Sample Output:
print(titleCasedWithLocale)  // Prints "The Quick Brown Fox"
```

## Deep Dive
Capitalization in programming has been around as long as we've had digital text processing - it's all about meeting user expectations. While `capitalized` in Swift standardizes strings to Title Case, where the first character of each word is uppercase, there are nuances.

Historically, programmers needed custom methods to capitalize, handling edge cases themselves. Swift’s `capitalized` takes a locale into account, which matters for proper nouns or locale-specific casing rules.

Speaking of alternatives, those dissatisfied with `capitalized` often turn to regex or write extensions on `String` for more complex rules. Implementation-wise, `capitalized` is essentially a built-in method that loops through the string, applying uppercase to the first letter after a non-letter character.

```Swift
extension String {
    func customCapitalized() -> String {
        return self.lowercased().replacingOccurrences(of: "\\b\\w", with: { 
            guard let firstChar = $0.first else { return $0 }
            return String(firstChar).uppercased() + $0.dropFirst()
        }, options: .regularExpression)
    }
}
```

The above extension uses a regular expression to capitalize the first letter of each word.

## See Also
For a deeper dive into Swift string manipulation, here are some helpful resources:
- [Swift Documentation on Strings](https://developer.apple.com/documentation/swift/string)
- [Ray Wenderlich's String Tutorial for Swift](https://www.raywenderlich.com/5492-working-with-strings-in-swift)
