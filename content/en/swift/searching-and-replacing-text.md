---
title:    "Swift recipe: Searching and replacing text"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

When writing code, it's common to encounter situations where you need to make changes to multiple instances of a particular text or phrase. Instead of manually finding and replacing each instance, using the Swift programming language's built-in search and replace function can save you time and effort.

## How To

Searching and replacing text in Swift is a straightforward process. First, declare a string variable containing the text you want to modify. For example:

```
let originalText = "Hello world!"
```

Next, use the `replacingOccurrences()` function to find and replace the text. This function takes two parameters: the text you want to search for, and the text you want to replace it with. For example, if we want to replace "world" with "universe", we would use the following code:

```
let modifiedText = originalText.replacingOccurrences(of: "world", with: "universe")
```

The resulting string will be "Hello universe!", with all instances of "world" replaced.

Another useful function for search and replace is `replacingOccurrencesOf()`. This function allows you to specify an optional range where you want the text to be replaced. For example, if we only want to replace the first instance of "world" in our string, we could use the following code:

```
let modifiedText = originalText.replacingOccurrences(of: "world", with: "universe", options: .literal, range: nil)
```

The `options` parameter allows us to specify whether the search should be case sensitive or not, and the `range` parameter allows us to specify a specific range in the string where we want the replacement to occur.

## Deep Dive

When performing a search and replace in Swift, it's important to understand a few key concepts. First, the functions we discussed earlier, `replacingOccurrences()` and `replacingOccurrencesOf()`, return a new string instead of modifying the existing one. This is because strings in Swift are immutable, meaning they cannot be changed after creation. This ensures data integrity and can prevent unexpected errors in your code.

Additionally, the `range` parameter in the `replacingOccurrencesOf()` function allows for more precise control over where the text should be replaced. You can specify a specific range of indices, or use other string manipulation techniques such as `prefix()` or `suffix()` to find a substring within the original string.

## See Also

- [Apple Developer Documentation on search and replace](https://developer.apple.com/documentation/swift/string/2894751-replacingoccurrences)
- [Hacking with Swift tutorial on searching strings](https://www.hackingwithswift.com/example-code/strings/how-to-search-for-a-substring-inside-a-string)
- [Swift by Sundell article on string manipulation](https://www.swiftbysundell.com/basics/strings/)