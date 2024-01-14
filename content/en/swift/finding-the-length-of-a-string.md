---
title:    "Swift recipe: Finding the length of a string"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
String manipulation is a fundamental aspect of programming, and one common task is finding the length of a string. By understanding how to find the length of a string, you will improve your ability to work with strings and become a more proficient Swift programmer.

## How To
Finding the length of a string may seem like a simple task, but there are a few different ways to achieve this in Swift. Let's take a look at some examples using the built-in String methods and properties.

First, we can use the `count` property to directly get the number of characters in a string:

```Swift
let message = "Hello, world!"
let length = message.count // 13
```

Another approach is to use the `characters` property, which returns the collection of characters in the string, and then get the `count` of that collection:

```Swift
let message = "Hello, world!"
let characters = message.characters
let length = characters.count // 13
```

Finally, we can also use the `count` method, which allows us to specify a range of characters to count. In this case, we will use the full range of the string to get the total length:

```Swift
let message = "Hello, world!"
let length = message.count(0..<message.count) // 13
```

As you can see, there are a few different ways to find the length of a string in Swift. It's important to note that in all of these examples, the count includes any whitespace characters as well.

## Deep Dive
Under the hood, Swift uses Unicode to represent strings, which means that a single character can have different representations depending on its context. This is why some languages or symbols may have a different count than others.

To go even deeper, the `count` property and method actually count the number of extended grapheme clusters in the string. This is a fancy way of saying it counts the number of visible characters, even if they are made up of multiple Unicode code points.

This becomes important when working with languages or symbols that are made up of multiple Unicode code points, such as emoji or accented characters. In these cases, the length of the string may not always match the number of actual characters in the string.

## See Also
- Official Apple Documentation on String Manipulation: https://developer.apple.com/documentation/swift/string
- Useful String Methods in Swift: https://medium.com/@abhimuralidharan/what-is-unicode-utf-8-utf-16-8a0f012cb292 
- Deep Dive into Swift Strings: https://www.hackingwithswift.com/quick-start/understanding-swift/what-are-strings