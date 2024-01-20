---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern enables Swift programmers to modify strings by removing certain sets of characters. It’s a common tool used for data cleaning, such as stripping out unwanted characters or whitespace.

## How to:

Here's an example of how you can do it in Swift. Suppose we want to delete all vowels from a string.

```Swift
let vowels: [Character] = ["a", "e", "i", "o", "u"]
var str = "Hello, playground"
str.removeAll(where: { vowels.contains($0.lowercased()) })
print(str)
```

The output will look like this:

```Swift
"Hll, plygrnd"
```

As you can see, the `removeAll(where:)` function plays a key role. It checks each character in the string and removes it if it matches any character in the vowel array.

## Deep Dive

Before Swift version 4.2, removing characters matching a pattern wasn’t as straightforward. Programmers had to use workarounds, such as converting strings to arrays and then filtering out the unwanted characters. The `removeAll(where:)` function was introduced in Swift 4.2 to provide an easier, more "Swift-like" way to perform this task.

An alternative method is using regular expressions, but they might be overkill for simple pattern matching. Moreover, they could hurt readability and performance, especially for large strings or complex patterns.

The key to `removeAll(where:)` is closure, a self-contained block of functionality that Swift lets you pass around in your code. In this context, the closure is a function that determines whether a character should be removed or not.

## See Also

Apple’s Swift Programming Language Guide provides in-depth information on strings and their manipulations: [Apple's Swift String Documentation](https://developer.apple.com/documentation/swift/string)

For a better understanding of closures in Swift, consider this well-detailed tutorial: [Understanding Swift Closures](https://www.hackingwithswift.com/articles/27/understanding-swift-closures)