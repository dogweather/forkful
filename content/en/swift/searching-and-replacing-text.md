---
title:                "Swift recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

When coding in Swift, it is common to encounter situations where you need to search and replace certain text within your code. This can be a tedious task if done manually, but luckily Swift has built-in methods that make this process much easier.

## How To

To search and replace text in Swift, we will be using the `.replacingOccurrences(of:with:)` method. This method takes two parameters: the text you want to replace and the text you want to replace it with. Let's take a look at an example:

```Swift
var str = "Hello World"
str.replacingOccurrences(of: "World", with: "Universe")
```

The above code will replace the text "World" with "Universe" in the `str` variable, resulting in a new string "Hello Universe". Notice that the method does not change the original string, it instead returns a new string with the replaced text.

We can also use this method to replace multiple occurrences of text by passing in an additional parameter, `options`. This allows us to specify things like case-sensitivity and search direction. Here's an example:

```Swift
var longStr = "The quick brown fox jumps over the lazy dog"
longStr.replacingOccurrences(of: "o", with: "e", options: [.caseInsensitive, .backwards])
```

In the above code, we are replacing all occurrences of the letter "o" with "e" in the `longStr` variable while ignoring case and searching from back to front. This will result in the new string "The quick breen fox jumps ever the lazy deg".

## Deep Dive

Under the hood, the `.replacingOccurrences(of:with:)` method uses regular expressions to find and replace text. Regular expressions are a powerful tool for pattern matching and can also be used in other areas of Swift, such as validation and string manipulation.

In addition to the `.replacingOccurrences(of:with:)` method, there are also other methods available for searching and replacing text in Swift, such as `.replacingCharacters(in:with:)` and `.replaceSubrange(_:with:)`. These methods also use regular expressions to perform their tasks.

It is important to note that when using regular expressions, certain characters like backslashes and parentheses need to be escaped with an additional backslash in order to be used in the search text.

## See Also

- [Apple Developer Documentation: String Protocol](https://developer.apple.com/documentation/swift/string)
- [Regular Expressions in Swift](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift)