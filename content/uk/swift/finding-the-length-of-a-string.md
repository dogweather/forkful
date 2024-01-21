---
title:                "Визначення довжини рядка"
date:                  2024-01-20T17:48:30.828742-07:00
model:                 gpt-4-1106-preview
simple_title:         "Визначення довжини рядка"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Finding the length of a string in Swift is about getting the count of characters it contains. Programmers do this to validate inputs, size elements, manage loops, and more. It's a bread-and-butter operation for almost any app dealing with text.

## How to (Як це зробити):
Swift makes checking a string's length straightforward. Use the `count` property of a string instance. Here's how:

```swift
let greeting = "Вітаю"
let length = greeting.count
print("The length of the string is \(length)")
```

Sample Output:

```
The length of the string is 6
```

Strings with complex characters, like emojis, are handled correctly:

```swift
let emojiGreeting = "Hello 👋"
print("The emoji string length is \(emojiGreeting.count)")
```

Sample Output:

```
The emoji string length is 7
```

## Deep Dive (Поглиблений Огляд):
Historically, string length calculation could trip up new developers due to Unicode complexities. In some languages, it was not a simple property access.

Swift's `String` type is Unicode-compliant. This means `.count` gives you the number of user-perceived characters, known as grapheme clusters, not the underlying code units or bytes. Characters outside the Basic Multilingual Plane, like emojis, are treated as single characters despite being made up of multiple Unicode scalars.

Alternatives? Before `count`, Swift programmers might have relied on `NSString`'s `length` property, which returns the UTF-16 code units count. This method is not Swift-native and can misrepresent the actual character count.

Implementation-wise, Swift's `String` keeps track of the character count as you modify the string, so accessing `.count` is an O(1) operation – fast and independent of the string's length.

## See Also (Дивіться також):
To go beyond the basics:

- Swift Documentation on Strings: [Swift.org Documentation](https://www.swift.org/documentation/#the-swift-programming-language)
- Deeper look at Unicode and Swift Strings: [Swift's String Manifesto](https://github.com/apple/swift/blob/main/docs/StringManifesto.md)
- Understanding grapheme clusters: [Unicode Text Segmentation](http://unicode.org/reports/tr29/)