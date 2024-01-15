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

## Why
Deleting characters from a string is a common task in programming, especially when dealing with user input or manipulating data. Using a pattern to delete specific characters can make the task more efficient and precise, saving time and reducing errors.

## How To
To delete characters from a string using a pattern in Swift, we can use the `replacingOccurrences(of:with:options:range:)` method of the `String` class. The syntax for this method is as follows:

```Swift
str.replacingOccurrences(of: pattern, with: replacement, options: options, range: range)
```

- `str` is the original string from which we want to delete characters.
- `pattern` is the pattern of characters we want to delete.
- `replacement` is the replacement string that will replace the deleted characters.
- `options` are the options for the matching behavior, such as case-insensitive or regular expression matching (provided as a `String.CompareOptions` value).
- `range` is the range of the original string where we want to perform the deletion. If not provided, it defaults to the entire string.

For example, let's say we have a string `str` with the value "Hello, world!". If we want to delete all the vowels from this string, we can do so with the following code:

```Swift
let str = "Hello, world!"
let pattern = "[aeiouAEIOU]"
let options: String.CompareOptions = [.regularExpression, .caseInsensitive]
let result = str.replacingOccurrences(of: pattern, with: "", options: options)
print(result) // Hll, wrld!
```

As we can see, the vowels have been successfully deleted from the string and the resulting string is printed with only the consonants remaining.

## Deep Dive
The `replacingOccurrences(of:with:options:range:)` method uses regular expressions to match the pattern and delete the corresponding characters from the string. Regular expressions are powerful tools for pattern matching, but can be difficult to understand at first.

In our example, the pattern used was "[aeiouAEIOU]", which represents a character class for all vowels in both lowercase and uppercase. The `options` parameter specifies that we want to perform a case-insensitive regular expression match.

If the `options` parameter is not provided, the method will default to case-sensitive exact matching. This means that only the characters in the pattern will be deleted, and the case of the characters in the string will be preserved. For example, if we use the same code as above but omit the `options` parameter, the result will be "HllO, wrlD!".

Regular expressions can be complex, but they provide a powerful way to match and manipulate patterns in strings. For a deeper dive into regular expressions in Swift, check out Apple's documentation on [Regular Expressions in Swift](https://developer.apple.com/library/archive/documentation/General/Reference/SwiftStandardLibraryReference/StringStructure.html#//apple_ref/swift/struct/RegularExpression).

## See Also
- [Apple Developer Documentation for String](https://developer.apple.com/documentation/swift/string)
- [A Beginner's Guide to Regular Expressions in Swift](https://www.raywenderlich.com/86205/nsregularexpression-swift-tutorial) 
- [Regular Expressions Cheat Sheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)