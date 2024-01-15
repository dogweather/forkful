---
title:                "Using regular expressions"
html_title:           "Swift recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why
If you've ever had to search for specific patterns or substrings within a large string of text, you know how tedious and time-consuming it can be. Regular expressions allow you to quickly and accurately find and manipulate text based on specific patterns, making them essential for tasks such as data validation and text parsing.

## How To
Regular expressions in Swift follow the same syntax and rules as other languages, including the use of special characters and modifiers to define patterns. Let's take a look at some coding examples to understand how regular expressions work in Swift.

To start, we'll need to import the `Foundation` framework, which contains the `NSRegularExpression` class and its methods for working with regular expressions. Once we have that imported, we can initialize a regular expression object, passing in our pattern as a string. For example:

```Swift
import Foundation

let pattern = "Hello"
let regex = try NSRegularExpression(pattern: pattern)
```

We can then use the `matches(in:options:range:)` method to find all the occurrences of our pattern within a given string. This method returns an array of `NSTextCheckingResult` objects, which contain details about the matched patterns. Let's see it in action with an input string that contains multiple instances of "Hello":

```Swift
let input = "Hello there! How are you? Hello again!"

let matches = regex.matches(in: input, options: [], range: NSRange(location: 0, length: input.count))

for match in matches {
    print(input[Range(match.range, in: input)!])
}
```

This will print out both occurrences of "Hello" in our input string. We can also use regular expressions to replace text within a string, using the `stringByReplacingMatches(in:options:range:withTemplate:)` method. For example, to replace all instances of "Hello" with "Hi", we could do the following:

```Swift
let output = regex.stringByReplacingMatches(in: input, options: [], range: NSRange(location: 0, length: input.count), withTemplate: "Hi")

print(output) // Hi there! How are you? Hi again!
```

There are many more methods and modifiers available for working with regular expressions in Swift, making it a powerful tool for text manipulation and parsing. 

## Deep Dive
Regular expressions are defined using a combination of normal characters and special characters. For example, the "Hello" pattern we used above is a simple regular expression that will only match the exact string "Hello". But most of the time, we need to match more complex patterns, such as specific word patterns or numbers.

To define these more complex patterns, we can use special characters such as `*`, `+`, and `?` to represent different types of matches. For example, the expression `"He*l+o?"` will match "Hello", "Heello", and "Helllo", but not "Helo" or "Hellloo". We can also use groups, which are enclosed in parentheses, to specify repetitions or alternative matches within a pattern.

There are also modifiers that we can add to our regular expressions to make them more specific. For example, the `^` modifier at the beginning of a pattern means the match must start at the beginning of the string, and the `$` modifier at the end means the match must end at the end of the string. There are many other modifiers available, each of which can change the behavior of your regular expressions.

## See Also
- [NSRegularExpression Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Regular Expressions Tutorial by Hacking with Swift](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift)
- [Regex101](https://regex101.com/), a useful tool for testing and creating regular expressions.