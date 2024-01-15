---
title:                "Finding the length of a string"
html_title:           "Swift recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
Strings are an essential part of any programming language, and knowing the length of a string can be incredibly useful for a variety of reasons. Whether you are building an app, creating a website, or just learning Swift, being able to find the length of a string is a fundamental skill that will come in handy.

## How To
Finding the length of a string in Swift is a simple and straightforward process. First, let's take a look at how to create a string variable:

```Swift
let myString = "Hello World!"
```

Now, to find the length of this string, we can use the built-in `count` method. Here's an example:

```Swift
print(myString.count) // Output: 12
```

As you can see, the `count` method returns the total number of characters in the string, including any spaces or punctuation.

We can also use the `count` method on a string variable that is not explicitly defined. For example:

```Swift
let name = "John"
print(name.count) // Output: 4
```

Lastly, we can also use the `count` method on string literals:

```Swift
print("Hello".count) // Output: 5
```

## Deep Dive
Now, let's take a deeper dive into finding the length of a string in Swift. Behind the scenes, the `count` method is using a property called `unicodeScalars` to calculate the length of the string. This property breaks down the string into its individual characters and counts them.

Additionally, you can also use the `unicodeScalars` property directly to get a more in-depth view of the characters in a string. Here's an example:

```Swift
let myString = "Swift is awesome!"
print(Array(myString.unicodeScalars)) // Output: ["S", "w", "i", "f", "t", " ", "i", "s", " ", "a", "w", "e", "s", "o", "m", "e", "!"]
```

This is especially helpful when working with multilingual strings that may contain characters from different languages.

## See Also
- [Apple Official Documentation on Strings in Swift](https://developer.apple.com/documentation/swift/string)
- [String Manipulation in Swift: Tips and Tricks](https://medium.com/better-programming/swift-string-manipulation-tips-and-tricks-66e65ac172e7)
- [Learn Swift: Strings](https://www.hackingwithswift.com/read/0/overview)