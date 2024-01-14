---
title:    "Swift recipe: Extracting substrings"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why Extracting Substrings is Useful

Substring extraction is a commonly used operation in programming, and Swift offers a variety of tools to make this task easier. Whether you need to manipulate strings for data processing or user input validation, extracting substrings can save you time and effort.

## How To Extract Substrings in Swift

To extract substrings in Swift, we can use the `substring` method or the `range` property. Let's take a look at some coding examples:

```Swift
let word = "Hello World"
let startIndex = word.startIndex
let endIndex = word.index(startIndex, offsetBy: 4)
let extractedSubstring = word[startIndex...endIndex] // Output: "Hello"
```

In this example, we first declare a string variable `word` with the value "Hello World". Then, we use `startIndex` to indicate the beginning of the string and `endIndex` to indicate the end of the substring we want to extract. Finally, we use `substring` with the range `startIndex...endIndex` to extract the substring "Hello".

Another way to extract substrings in Swift is by using the `range` property:

```Swift
let sentence = "I love learning Swift"
let range = sentence.range(of: "love")!
let extractedSubstring = sentence[range] // Output: "love"
```

This method uses the `range(of: )` function to specify the substring we want to extract, and then we use the `range` property to extract the substring "love".

## Deep Dive into Extracting Substrings

There are a few things to keep in mind when extracting substrings in Swift. The first is that `substring` and `range` both use zero-based indexing. This means that the first character in a string is at index 0, the second character is at index 1, and so on.

Additionally, the `substring` method is inclusive of both the start and end indices, while the `range` property is not. This means that if we use `substring` with the range `startIndex...endIndex`, the resulting substring will include the character at the end index. However, if we use `range` with the same range, the resulting substring will not include the character at the end index.

Furthermore, Swift also offers the `prefix` and `suffix` methods for extracting substrings from the beginning and end of a string, respectively. These methods take in a parameter indicating the number of characters to extract and return a substring of that length.

## See Also

- [String Manipulation with Swift](https://www.hackingwithswift.com/articles/162/how-to-use-string-interpolation-in-swift)
- [Working with Substrings in Swift](https://www.appcoda.com/swift-string-substring/)
- [Swift String Cheat Sheet](https://academy.realm.io/posts/tryswift-mikkoswift-string-cheat-sheet/)