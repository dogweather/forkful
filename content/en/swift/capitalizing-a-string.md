---
title:                "Swift recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing a string may seem like a simple task, but it can be a useful tool in string manipulation. It allows for easier readability and presentation of data to the user. In this blog post, we will explore the different methods of capitalizing a string in Swift.

## How To

First, let's start with the most basic way of capitalizing a string using the `capitalized` property. This will capitalize the first letter of each word in the string.

```Swift
let message = "hello world"
print(message.capitalized)
```

Output: "Hello World"

If you want to capitalize only the first letter of the entire string, you can use the `capitalizedFirst` method.

```Swift
let message = "hello world"
print(message.capitalizedFirst)
```

Output: "Hello world"

If you want to capitalize every letter in the string, you can use the `uppercased` method.

```Swift
let message = "hello world"
print(message.uppercased)
```

Output: "HELLO WORLD"

Another method for capitalizing the first letter of each word is by using `components(separatedBy:)` and `map` functions.

```Swift
let message = "hello world"
let capitalizedWords = message.components(separatedBy: " ").map({String($0.prefix(1)).uppercased() + String($0.dropFirst())})
let newMessage = capitalizedWords.joined(separator: " ")
print(newMessage)
```

Output: "Hello World"

Lastly, you can also use the `String` initializer to capitalize a string. This method allows you to specify which letters to capitalize.

```Swift
let message = "hello world"
let newMessage = String(message.prefix(1)).uppercased() + String(message.dropFirst())
print(newMessage)
```

Output: "Hello world"

## Deep Dive

As we can see, there are many different ways to capitalize a string in Swift. When using the `uppercased` method, it is important to note that it will capitalize all letters in the string, including special characters and numbers. This may not be desired in certain situations.

When using the `capitalized` method, it is important to keep in mind that it capitalizes the first letter of each word, so words like "iPhone" or "McDonald's" will be capitalized incorrectly.

Using the `String` initializer method allows for more control over which letters to capitalize, but it can be more lengthy and may not be suitable for large strings.

## See Also

For more information on string manipulation in Swift, check out these resources:

- [Apple's Swift String Documentation](https://developer.apple.com/documentation/swift/string)
- [Hacking with Swift's String Capitlization Tutorial](https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-the-first-letter-of-a-string)
- [Swift Education's Manipulating Strings in Swift Tutorial](https://www.simpleswiftguide.com/manipulating-strings-in-swift-tutorial/)