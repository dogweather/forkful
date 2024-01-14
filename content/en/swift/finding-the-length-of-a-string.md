---
title:    "Swift recipe: Finding the length of a string"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Why
Are you learning Swift and wondering why you would ever need to know the length of a string? Well, let me tell you, it's a common task in programming and can come in handy for a variety of reasons. From validating user input to manipulating text, understanding how to find the length of a string is a fundamental skill in Swift.

## How To
To find the length of a string in Swift, there are a few different methods you can use depending on your specific needs. Let's take a look at some examples and their corresponding output.

```Swift
// Example 1: Using the built-in count property
let string1 = "Hello, world!"
let length1 = string1.count // Output: 13
```

```Swift
// Example 2: Iterating through each character of the string
let string2 = "Coding is fun!"
var length2 = 0

for _ in string2 {
    length2 += 1
}

// Output: 15
```

```Swift
// Example 3: Removing whitespace and punctuation before counting
let string3 = "    Swift is #awesome!     "
let string3Trimmed = string3.trimmingCharacters(in: .whitespacesAndNewlines)
let length3 = string3Trimmed.count // Output: 16
```

It's important to note that the first two examples, while simpler, may not always be accurate. The count property counts the number of UTF-8 code units in a string, which may not always correspond to the actual characters. Using the for loop and removing whitespace and punctuation allows for a more accurate count of visible characters.

## Deep Dive
Behind the scenes, the count property is actually accessing the count property of the string's underlying UTF-16 representation. This representation can cause issues when working with certain languages such as Chinese or Emojis which require multiple code units. To accurately handle these situations, you can use the String's lengthOfBytes(using:) method.

Another important aspect to consider when finding the length of a string is performance. The count property is O(1), meaning it takes the same amount of time regardless of the size of the string. However, the for loop and trimming methods have a time complexity of O(n) where n is the length of the string. Therefore, for larger strings, it may be more efficient to use the count property.

## See Also
- [Official Apple Documentation on Strings](https://developer.apple.com/documentation/swift/string)
- [Tutorial on Using Strings in Swift](https://www.raywenderlich.com/5540...#toc-anchor-009)
- [Stack Overflow Discussion on Calculating String Length in Swift](https://stackoverflow.com/questions/24068864/get-the-count-of-characters-in-a-string-swift)