---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? 

Extracting substrings is a process used to obtain a part of a string. It’s an essential operation in a programmer's arsenal as it enables data manipulation for tasks like parsing text, formatting output, or search operations.

## How to:

In Swift, extracting a substring can be accomplished using the `subscript` property of a string. The `subscript` property uses a `range` to define the boundaries of the substring.

```Swift
let str = "Hello World" 
let startIndex = str.index(str.startIndex, offsetBy: 6)
let endIndex = str.index(str.endIndex, offsetBy: -1)
let substr = str[startIndex...endIndex] 
print(substr) 
```
**Sample Output**

```
World
```

## Deep Dive:

(1) **Historical context**: Before Swift 4, extracting substrings was somewhat unintuitive and verbose. The modern `substring(from:)` and `substring(to:)` methods now make this task a breeze.

(2) **Alternatives**: Apart from using range, Swift also provides handy functions like `prefix(_: )` and `suffix(_: )` for limiting the extraction of characters from the start and end of the string respectively.

```Swift
let str = "Welcome to Swift"
let prefix = str.prefix(7)
let suffix = str.suffix(5)
print("Prefix: \(prefix), Suffix: \(suffix)")
```
**Sample Output**

```
Prefix: Welcome, Suffix: Swift
```

(3) **Implementation details**: With Swift 4 and later, substring types retain a reference to the entire original string, leading to efficient memory use. However, be aware that an unintentionally retained substring might cause unexpected memory consumption if the original string is large and kept alive.

## See Also:

For additional reading and exploration regarding Swift substring operations, check out these links:

1. [Apple’s Swift Documentation on Strings](https://developer.apple.com/documentation/swift/string)
2. [Swift Standard Library](https://developer.apple.com/documentation/swift/swift_standard_library/)
3. [Swift by Sundell](https://www.swiftbysundell.com/basics/strings/)
4. [Ray Wenderlich Swift Strings Tutorial](https://www.raywenderlich.com/449-swift-strings-tutorial)
5. [Swift Substrings in HackingWithSwift](https://www.hackingwithswift.com/example-code/strings/what-is-a-substring)