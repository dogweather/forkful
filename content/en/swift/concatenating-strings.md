---
title:    "Swift recipe: Concatenating strings"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why Concatenating Strings in Swift is Useful

Concatenating strings is a useful skill for any Swift programmer to have. It allows you to combine multiple strings into one, making it easier to manipulate and present data in your code. Whether you're building a simple app or a complex program, knowing how to concatenate strings can make your code more efficient and user-friendly.

## How To Concatenate Strings in Swift

To concatenate strings in Swift, you can simply use the "+" operator between two strings. Here's an example:

```Swift
let firstName = "John"
let lastName = "Smith"

let fullName = firstName + " " + lastName
```

The resulting string will be "John Smith". You can also concatenate strings with variables, making it dynamic and versatile. Here's another example:

```Swift
let greeting = "Hello"
var name = "Sarah"

let message = greeting + ", " + name + "!"
```

The output will be "Hello, Sarah!". As you can see, concatenating strings allows you to create more personalized and natural messages in your code.

## Deep Dive into Concatenating Strings

In Swift, you can not only concatenate variables and strings, but also other types such as integers and floating-point numbers. However, it's important to note that the result will be a string and not a numerical value. Concatenating strings can also be done using the `append()` method, which is useful for adding characters to the end of existing strings.

Another important aspect to understand is the use of string interpolation, denoted by a backslash and parentheses. This allows you to insert variables or expressions directly into a string without concatenating. Here's an example:

```Swift
let age = 25
let message = "I am \(age) years old."
```

The output will be "I am 25 years old." Using string interpolation can save you time and make your code cleaner and more organized.

## See Also

To learn more about concatenating strings in Swift, check out these helpful resources:

- [Apple's Official String Interpolation Documentation](https://developer.apple.com/documentation/swift/string_interpolation)
- [How Concatenating Strings Works in Swift](https://www.hackingwithswift.com/sixty/3/3/how-we-use-operators-for-strings)
- [Advanced String Concatenation Techniques in Swift](https://medium.com/swift-programming/advanced-string-concatenation-in-swift-cf0fe78c23db)