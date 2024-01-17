---
title:                "Deleting characters matching a pattern"
html_title:           "Swift recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Deleting characters that match a specific pattern is a common task in programming, especially when dealing with strings. This means removing any characters from a string that meet a certain criteria, such as being a number or a specific letter. Programmers often do this to clean up data or manipulate strings for various purposes.

## How to:
To delete characters matching a pattern in Swift, we can use the `replacingOccurrences(of:with:)` method. First, we need to create a string that contains the characters we want to remove. Then, we can use this method to replace those characters with an empty string, effectively deleting them from the original string. Here's an example:

```Swift
let myString = "Hello, 123 World!"
let pattern = "[0-9a-zA-Z]" //this will match any number or letter
let strippedString = myString.replacingOccurrences(of: pattern, with: "") //this will remove all numbers and letters from the string
print(strippedString) //prints ", !" since those were the only characters matching our pattern
```

## Deep Dive
The `replacingOccurrences(of:with:)` method was introduced in Swift 4, making it easier to delete characters matching a pattern without having to resort to complex string manipulation methods. Before this, programmers had to use `NSRegularExpression` or other Objective-C methods to achieve the same result. However, this also means that this method is not available in older versions of Swift.

An alternative way to delete characters matching a pattern is by using regular expressions, which allows for more advanced pattern matching. However, this can be more complicated and may not be necessary for simple cases.

Internally, the `replacingOccurrences(of:with:)` method uses the `range(of:options:range:locale:)` method to find the range of characters that match the given pattern. It then replaces those characters with the specified replacement string.

## See Also
- Apple's official documentation on [`replacingOccurrences(of:with:)`](https://developer.apple.com/documentation/swift/string/1642993-replacingoccurrences) method
- [Regular Expressions in Swift](https://www.appcoda.com/swift-string/) tutorial by AppCoda