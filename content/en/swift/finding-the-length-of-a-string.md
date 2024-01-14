---
title:                "Swift recipe: Finding the length of a string"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

If you're new to Swift programming, you may be wondering why it's necessary to find the length of a string. Well, knowing the length of a string can be useful for a variety of tasks such as validating user input, truncating strings, or even for simple data analysis. In this post, we'll explore how to find the length of a string in Swift.

## How To

To find the length of a string in Swift, we can simply use the `count` property. Let's take a look at an example:

```Swift
let str = "Hello World"
let length = str.count
print("The length of the string is \(length)")
```

This will output:

```
The length of the string is 11
```

As you can see, the `count` property gives us the exact number of characters in the string, including whitespaces. It's worth noting that the `count` property returns an `Int` value, so we can use it in our calculations if needed.

We can also use `count` in conjunction with other string methods. For instance, we can find the number of uppercase letters in a string like this:

```Swift
let str = "Hello World"
let uppercaseCount = str.filter { $0.isUppercase }.count
print("The number of uppercase letters in the string is \(uppercaseCount)")
```

This will output:

```
The number of uppercase letters in the string is 2
```

## Deep Dive

Internally, Swift uses Unicode to represent strings, which means that it can handle characters from various languages and scripts. This also means that the `count` property takes into account the individual Unicode characters in the string, which may differ from the number of visible characters.

For example, the string "💻😃" contains two visible characters but actually has a `count` of 2 because the emojis are represented by a combination of Unicode characters.

Understanding this difference is important when working with strings that contain emojis or characters from non-Latin languages.

## See Also

- [String - Swift Standard Library | Apple Developer Documentation](https://developer.apple.com/documentation/swift/string)
- [Unicode in Swift - Swift Blog | Apple Developer](https://developer.apple.com/swift/blog/?id=29)
- [Basic Operators - The Swift Programming Language | Apple Developer Documentation](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html)