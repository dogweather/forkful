---
title:    "Swift recipe: Using regular expressions"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

### Why Regular Expressions Can Make Your Swift Programming Easier

Regular expressions may seem intimidating at first, but incorporating them into your Swift programming can make your life a whole lot easier. They allow you to search for specific patterns within a string, making tasks like input validation, data extraction, and text manipulation more efficient and less time-consuming. Plus, once you get the hang of using regular expressions, you'll wonder how you ever managed without them.

### How To Use Regular Expressions in Swift

To use regular expressions in Swift, we first need to import the `Foundation` framework. Then, we can create a `NSRegularExpression` object with a pattern and options. Let's say we want to check if a string contains three consecutive digits. We can do this with the following code:

```Swift
import Foundation

let pattern = "[0-9]{3}" // pattern for 3 consecutive digits
let string = "abc321def"

do {
    let regex = try NSRegularExpression(pattern: pattern, options: [])
    let range = NSRange(location: 0, length: string.utf16.count)
    let matches = regex.matches(in: string, range: range)
    print(matches.count) // output: 1 match found
} catch {
    print("Invalid pattern: \(error)")
}
```

This code creates a regular expression that matches three consecutive digits and uses it to search for matches within the string "abc321def". In this case, we get an output of 1 match found.

We can also use regular expressions to replace parts of a string. Let's say we want to replace all occurrences of "swift" with "Swift Programming". We can do this with the following code:

```Swift
let pattern = "swift"
let string = "I love swift programming, it's so fun!"

do {
    let regex = try NSRegularExpression(pattern: pattern, options: [])
    let range = NSRange(location: 0, length: string.utf16.count)
    let newString = regex.stringByReplacingMatches(in: string, range: range, withTemplate: "Swift Programming")
    print(newString) // output: "I love Swift Programming programming, it's so fun!"
} catch {
    print("Invalid pattern: \(error)")
}
```

These are just a few simple examples, but regular expressions can be used for much more complex tasks as well.

### Deep Dive into Regular Expressions in Swift

Regular expressions in Swift follow the same rules as regular expressions in other programming languages. Some common metacharacters and their meanings are:

- `^` - matches the beginning of a string
- `$` - matches the end of a string
- `.` - matches any single character
- `*` - matches zero or more occurrences of the previous character
- `?` - matches zero or one occurrence of the previous character
- `+` - matches one or more occurrences of the previous character
- `[ ]` - matches any character within the brackets
- `[^ ]` - matches any character not within the brackets
- `()` - groups characters together

It's also important to note that regular expressions are case-sensitive by default, but this can be changed with options like `NSRegularExpression.Options.caseInsensitive`.

See Also

- [NSRegularExpression Class Reference](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [How to Use Regular Expressions in Swift](https://www.raywenderlich.com/5765-how-to-use-regular-expressions-in-swift)
- [Regular Expressions Cheat Sheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)