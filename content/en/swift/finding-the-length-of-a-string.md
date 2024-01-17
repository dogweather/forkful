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

## What & Why?

Finding the length of a string is a common task in programming that involves determining the number of characters in a given string. This information can be useful in a variety of scenarios, such as validating user input, manipulating data, or formatting output.

## How to:

To find the length of a string in Swift, we can use the built-in `count` method. First, we define a string variable. Then, we use the `count` method and assign the result to another variable. Here's an example:

```Swift
let myString = "Hello, world!"
let length = myString.count

print("The length of the string is \(length)") // Output: The length of the string is 13
```

In the above example, we defined a string variable `myString` and assigned the value `"Hello, world!"` to it. Then, we used the `count` method on the string and assigned the result, which is the length of the string, to the variable `length`. Finally, we printed the string and its length using string interpolation.

## Deep Dive

Since strings are an important part of programming, the concept of string length has been around since the early days of programming. In Swift, the `count` method replaced the older `length` method that was used in Objective-C. The `count` method is also used to determine the number of items in other data types such as arrays and dictionaries.

There are other ways to find the length of a string, such as using a loop and counting the characters, but the `count` method is the most efficient and recommended way in Swift. Additionally, different languages and programming environments may have their own methods for finding the length of a string.

Under the hood, the `count` method in Swift uses the log2 sub-structure of a string's `utf8` view, making it a constant-time operation to find the length of a string. This means that regardless of the length of the string, the time to determine its length remains constant.

## See Also

To learn more about strings and the `count` method in Swift, check out the [official documentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html) on Strings and Characters. You can also explore other ways to manipulate strings in Swift, such as accessing a specific character or replacing parts of a string.

For more in-depth information about Swift and its features, the [Swift Programming Language](https://docs.swift.org/swift-book/) book is a great resource. And if you're interested in learning more about string length and its history, Wikipedia has a [detailed article](https://en.wikipedia.org/wiki/String_(computer_science)#:~:text=The%20length%20of%20a%20string,to%20commonly%20used%20string%20functions.) on the topic. Happy coding!