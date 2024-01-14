---
title:    "Swift recipe: Searching and replacing text"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Why

If you're a Swift programmer, you've probably had to deal with text at some point. Whether it's input from a user, data from an API, or just string manipulation, text is an essential part of programming. But what happens when you need to replace certain words or phrases within a string? In this blog post, we'll explore the why, how, and deeper aspects of searching and replacing text in Swift.

## How To

In Swift, there are a few different ways to search and replace text. One common method is by using the `replacingOccurrences(of:with:)` function. Let's take a look at an example:

```Swift
let sentence = "I love programming in Swift!"
let newSentence = sentence.replacingOccurrences(of: "Swift", with: "Python")
print(newSentence)
```

This code will replace the word "Swift" with "Python" in the `sentence` variable, and the output will be "I love programming in Python!". Notice how we use the dot notation to call the `replacingOccurrences` function on the `sentence` variable.

Another option is to use the `replacingOccurrences(of:with:options:)` function, which allows us to specify options for the replacement. For example, we can choose to replace only the first occurrence, ignore case sensitivity, or use regular expressions. Let's look at an example:

```Swift
let text = "I'm learning Swift!"
let newText = text.replacingOccurrences(of: "swift", with: "Python", options: .caseInsensitive)
print(newText)
```

This code will ignore the capitalization of "Swift" and replace it with "Python", resulting in the output "I'm learning Python!".

## Deep Dive

So far, we've only looked at basic examples of replacing strings in Swift. But there's a lot more to it than just simple replacements. For instance, we can use the `range(of:)` function to get the range of a specific word or phrase within a string. Then, we can use the `replaceSubrange(_:with:)` function to directly replace that portion of the string. Let's see how it works:

```Swift
var text = "Swift is a powerful language!"
if let range = text.range(of: "powerful") {
    text.replaceSubrange(range, with: "amazing")
}
print(text)
```

This code will replace the word "powerful" with "amazing", resulting in the output "Swift is an amazing language!". Notice how we had to use the `if let` statement to safely unwrap the optional range variable before using it.

See Also

- [Apple Documentation on replacingOccurrences](https://developer.apple.com/documentation/foundation/nsstring/1411945-replacingoccurrences)
- [More advanced string manipulation in Swift](https://www.swiftbysundell.com/basics/string-manipulation/)
- [Using regular expressions in Swift](https://www.raywenderlich.com/86205/nsregularexpression-swift-tutorial)