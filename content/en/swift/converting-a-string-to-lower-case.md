---
title:                "Swift recipe: Converting a string to lower case"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case is a common task in Swift programming. By converting a string to lower case, we can ensure consistency in our data and make string comparisons easier. It also allows us to manipulate strings in a way that is more readable and user-friendly.

## How To

To convert a string to lower case in Swift, we can use the `lowercased()` method. This method takes the string and returns a new string with all characters converted to lower case. Let's see an example of how we can use this method:

```Swift
let greeting = "Hello World"
let lowercasedGreeting = greeting.lowercased()

print(lowercasedGreeting) // Output: hello world
```

As we can see, the string "Hello World" is converted to lower case and stored in the new variable `lowercasedGreeting`.

## Deep Dive

It's important to note that the `lowercased()` method will only convert characters that have an uppercase equivalent. This means that any characters that do not have a lower case equivalent, such as emoji or special characters, will not be affected by this method.

Additionally, the `lowercased()` method is dependent on the character set used by the device's operating system. This means that the output may vary on different devices or systems.

Another way to convert a string to lower case is by using the `caseInsensitiveCompare()` method. This method compares two strings regardless of their case and can be useful when sorting strings alphabetically.

## See Also

- Apple Developer Documentation: [String.lowercased()](https://developer.apple.com/documentation/swift/string/2997157-lowercased)
- Swift by Sundell: [Converting text case in Swift](https://www.swiftbysundell.com/tips/converting-text-case-in-swift/)
- Hacking with Swift: [How to convert the case of a string using localized capitalization](https://www.hackingwithswift.com/example-code/strings/how-to-convert-the-case-of-a-string-using-localized-capitalization)