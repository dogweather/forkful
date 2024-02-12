---
title:                "Finding the length of a string"
aliases: - /en/swift/finding-the-length-of-a-string.md
date:                  2024-01-20T17:48:08.422126-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finding the length of a string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Finding the length of a string means figuring out how many characters it contains. Programmers do this to validate input, manipulate text, or simply to understand the size of their data.

## How to:
In Swift, you get a string's length by accessing its `count` property. Straightforward, let's do it:

```Swift
let greeting = "Hello, World!"
print(greeting.count) // Output: 13
```

Remember that Swift considers emoji as single characters, thanks to Unicode:

```Swift
let wave = "👋"
print(wave.count)  // Output: 1
```

## Deep Dive
Back in the Objective-C days, string length wasn't so direct—there was `length` and `lengthOfBytes(using:)`. Swift made it cleaner with `count`.

Be aware of composite characters: visually single characters made of multiple Unicode scalars. `count` handles these gracefully.

Alternatives? Sure, you could traverse the string with a loop, but that's re-inventing the wheel and less efficient.

Under the hood, `count` is O(n), where ‘n’ is the number of characters. That’s because Swift’s `String` is not a collection of `Char`s, but a sequence of grapheme clusters, which can vary in length.

## See Also
- Swift Documentation on Strings: [Swift String Docs](https://developer.apple.com/documentation/swift/string)
- Unicode Basics: [Unicode Consortium](https://home.unicode.org)
- Dive into Swift’s String Performance: [Swift String Perf](https://swift.org/blog/utf8-string/)
