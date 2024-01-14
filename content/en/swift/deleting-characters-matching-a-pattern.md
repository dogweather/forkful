---
title:                "Swift recipe: Deleting characters matching a pattern"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Sometimes when working with strings in Swift, we come across a situation where we need to delete specific characters or patterns from the string. This could be for various reasons, such as cleaning up user input or manipulating data. In this blog post, we will explore the concept of deleting characters matching a pattern in Swift and how it can be useful in our programming.

## How To

To delete characters matching a pattern in Swift, we can use the `replacingOccurrences(of:with:)` method on a string. This method takes two parameters - the pattern we want to remove and the string we want to replace it with. Let's take a look at a simple example:

```Swift
let string = "Hello, World!"
let modifiedString = string.replacingOccurrences(of: ",", with: "")
print(modifiedString)
```

In the above code, we use the `replacingOccurrences(of:with:)` method to remove the comma from the string and replace it with an empty string, effectively deleting it. The output of this code will be `Hello World!` since the comma was removed.

We can also use this method to delete multiple characters matching a pattern. Let's see how we can remove all vowels from a string:

```Swift
let string = "Swift is awesome!"
let vowels = "aeiou"
var modifiedString = string
for char in vowels.characters {
    modifiedString = modifiedString.replacingOccurrences(of: String(char), with: "")
}
print(modifiedString)
```

Here, we first define a string containing all the vowels. Then we loop through each vowel and use the `replacingOccurences(of:with:)` method to remove it from the string. The output of this code will be `Swft s wsm!` since all vowels were deleted from the string.

## Deep Dive

Behind the scenes, the `replacingOccurrences(of:with:)` method uses regular expressions to find and replace patterns in a string. Regular expressions, or regex, is a powerful tool for pattern matching and manipulation in strings. It allows for more complex and dynamic pattern matching, such as using wildcards or patterns within the pattern.

For example, we can use regex to remove all alphabetic characters from a string:

```Swift
let string = "H3ll0, my n4m3 is John"
let modifiedString = string.replacingOccurrences(of: "[a-zA-Z]", with: "", options: .regularExpression)
print(modifiedString)
```

In the above code, we use a regex pattern inside the `replacingOccurrences` method to remove all alphabetic characters from the string. The output of this code will be `3 0, 4 3`.

## See Also

- [The Swift Programming Language: Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [NSHipster: Regular Expressions](http://nshipster.com/nsregularexpression/)
- [Swift.org: Regular Expressions](https://swift.org/blog/regex-and-strings/)