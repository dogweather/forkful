---
title:    "Swift recipe: Deleting characters matching a pattern"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

In programming, there are often situations where we need to manipulate strings. One common task is deleting certain characters from a string that match a specific pattern. This can be useful for tasks such as removing punctuation or formatting characters from user input.

## How To

To delete characters matching a pattern in Swift, we can use the `replacingOccurrences(of:with:)` method. This method takes in two parameters - the pattern to match and the replacement string. 

```Swift
//Sample input string
let input = "Hello, World!"

//Deleting all punctuation marks
let result = input.replacingOccurrences(of: "[^a-zA-Z]", with: "", options: .regularExpression, range: nil)
print(result)
//Output: HelloWorld 
```

In the above example, we use regular expressions to match any characters that are not letters and replace them with an empty string, effectively deleting them from the original string.

We can also use this method to delete specific characters, such as a particular letter or symbol, from a string. 

```Swift
//Sample input string
let input = "I love Swift!"

//Deleting the letter "S"
let result = input.replacingOccurrences(of: "S", with: "", options: [], range: nil)
print(result)
//Output: I love wift!
```

Regular expressions can also be used in more complex ways to match specific patterns and delete characters accordingly. For example, we can delete all numbers from a string:

```Swift
//Sample input string
let input = "Apples123 are great456"

//Deleting all numbers
let result = input.replacingOccurrences(of: "[0-9]", with: "", options: .regularExpression, range: nil)
print(result)
//Output: Apples are great
```

## Deep Dive

Regular expressions may seem intimidating at first, but they are a powerful and efficient tool for manipulating strings. The `[ ]` in a regular expression represents a character class, which can match any of the characters within the brackets. For example, `[a-zA-Z]` means any letter, `[0-9]` means any number, and `[^a-z]` means any character that is not a lowercase letter.

Using the `options` parameter in the `replacingOccurrences` method allows us to specify additional options for how the replacement is performed. In the examples above, we used the `.regularExpression` option to enable regular expression matching. Other options include `.caseInsensitive`, which ignores case when matching, and `.anchored`, which only matches characters at the beginning or end of the string.

## See Also

- [String and Character Manipulation in Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Regular Expressions in Swift](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Advanced Regular Expressions in Swift](https://www.swiftbysundell.com/articles/advanced-regular-expressions-in-swift/)