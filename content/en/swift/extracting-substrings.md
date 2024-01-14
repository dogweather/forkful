---
title:                "Swift recipe: Extracting substrings"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to extract a specific part of a string in your Swift code? Whether it's for formatting purposes or to retrieve important data, extracting substrings can be a useful skill for any Swift programmer. In this blog post, we will dive into the topic of substring extraction and learn how it can benefit us in our coding endeavors.

## How To

First, let's start with understanding what a substring is. A substring is a part of a larger string that can be extracted based on a specific set of criteria. In Swift, we can use the `subString(from:to:)` function to extract substrings. Let's see an example of how this works:

```Swift
var sentence = "I love coding in Swift!"
var substring = sentence.subString(from: 2, to: 6)
print(substring)
```
The output of this code will be "love", as we are extracting the characters from the 2nd index to the 6th index of the original string.

But what if we don't know the exact index of the characters we want to extract? In that case, we can use the `subString(from:)` function to extract all characters starting from a specific index to the end of the string. Let's see an example:

```Swift
var sentence = "I love coding in Swift!"
var substring = sentence.subString(from: 7)
print(substring)
```
The output of this code will be "coding in Swift!", as we are extracting all characters from the 7th index to the end of the string.

We can also extract substrings based on a specific pattern using the `range(of:)` function. For example, if we want to extract everything before the word "coding", we can do so with the following code:

```Swift
var sentence = "I love coding in Swift!"
var range = sentence.range(of: "coding")
var substring = sentence.subString(to: range!.lowerBound)
print(substring)
```

The output of this code will be "I love ", as we are extracting all characters before the word "coding". 

## Deep Dive

In Swift, substrings are represented using the `Substring` type, which is a thin wrapper around the original string. This means that when we extract a substring, it shares the same memory as the original string, making it more efficient than creating a new string.

It's also worth noting that substrings are not limited to just strings. We can extract substrings from any type that conforms to the `Collection` protocol, such as arrays and dictionaries.

Lastly, it's important to be aware that since substrings are just a portion of the original string, any changes made to the substring will also affect the original string. If we want to create a new string with the extracted substring, we can use the `String` initializer and pass in the substring as an argument.

## See Also

- [Swift Substrings: Explained with Code Examples](https://www.swiftbysundell.com/basics/substring/)
- [Substring in Swift: Explained with Code Examples](https://www.programiz.com/swift-programming/substring)
- [The Power of Substring in Swift](https://medium.com/@lucasfarah/using-the-power-of-substring-in-swift-60c297b7bc4a)