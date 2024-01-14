---
title:                "Swift recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Finding the length of a string may seem like a simple task, but it can actually be quite useful and essential in many programming scenarios. For example, when dealing with user input or manipulating data, it's important to know the length of a string in order to properly handle it. In this blog post, we're going to dive into the world of string length in Swift and explore different ways to find it.

## How To

To find the length of a string in Swift, we can use the `count` property of the string. Let's take a look at a simple example:

```Swift
let message = "Hello World!"
print(message.count)
// Output: 12
```

As you can see, we simply use the `count` property and print it out to get the length of the string. But what about empty strings? Let's try it out:

```Swift
let emptyString = ""
print(emptyString.count)
// Output: 0
```

As expected, the `count` property returns 0 for an empty string. But what happens when we have an emoji in our string? Let's find out:

```Swift
let smiley = "😊"
print(smiley.count)
// Output: 2
```

Surprisingly, the `count` property also counts emojis as 2 characters. This is because emojis are made up of Unicode characters, with some being represented by only 1 Unicode value and others by 2. Therefore, it's important to keep this in mind when working with strings that may contain emojis.

## Deep Dive

In Swift, strings are represented by the `String` type, which is a collection of individual characters. This means that finding the length of a string is actually finding the number of characters it contains. However, it's important to note that not all characters have the same length in terms of memory. For example, a single Unicode character can have a varying length in memory, depending on the encoding format used. This can result in the `count` property returning different values for strings with the same number of characters but different Unicode representations.

## See Also

- [Apple documentation on string length](https://developer.apple.com/documentation/swift/string/2894149-count)
- [Stack Overflow discussion on counting emoji characters](https://stackoverflow.com/questions/41938886/how-to-count-emoji-characters-in-string-in-swift)
- [Hacking with Swift article on string basics](https://www.hackingwithswift.com/quick-start/understanding-swift/whats-the-difference-between-a-unicode-character-a-glyph-and-a-code-point)