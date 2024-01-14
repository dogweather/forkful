---
title:                "Swift recipe: Capitalizing a string"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
Capitalizing strings in Swift may seem like a simple task, but it can have a big impact on the visual presentation of your code. By capitalizing strings, you can make your code more readable and organized, making it easier to navigate and understand.

## How To
To capitalize a string in Swift, you can use the `capitalized` property. This property converts the first character of each word in a string to uppercase, while keeping the rest of the characters lowercase.

```Swift
let name = "john doe" 

print(name.capitalized) // Output: John Doe
```

You can also use the `uppercased` and `lowercased` methods to convert the entire string to uppercase or lowercase respectively.

```Swift
let word = "hello world"

print(word.uppercased()) // Output: HELLO WORLD
print(word.lowercased()) // Output: hello world
```

Another option is to use the `capitalizingFirstLetter()` function, which capitalizes the first character of a string.

```Swift
func capitalizingFirstLetter(_ string: String) -> String {
    return string.prefix(1).capitalized + string.dropFirst()
}

print(capitalizingFirstLetter("hello")) // Output: Hello
```

## Deep Dive
It's important to note that the `capitalized` property and `uppercased`/`lowercased` methods only work on strings that are entirely composed of letters. If your string contains numbers or special characters, it will be left unchanged.

Additionally, these methods use the Unicode standard to determine capitalization, meaning they may not produce the desired results for certain characters in non-English languages.

If you want more control over capitalization, you can use the `replaceSubrange(_:with:)` method to manually replace specific characters in a string with their capitalized counterparts.

## See Also
- [Apple Documentation on Capitalizing Strings](https://developer.apple.com/documentation/swift/string/1774151-capitalized)
- [Hacking with Swift Article on Capitalizing Strings](https://www.hackingwithswift.com/example-code/strings/how-to-capitalise-the-first-letter-of-a-string)