---
title:                "Swift recipe: Using regular expressions"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are powerful tools that allow developers to search, extract, and manipulate text based on patterns. These patterns can range from simple characters to complex sequences, providing a versatile approach to working with textual data. If you want to take your Swift programming skills to the next level and make your code more efficient, then learning how to use regular expressions is a must.

## How To

Using regular expressions in Swift involves importing the Foundation framework and utilizing the `NSRegularExpression` class. Let's take a look at a simple example that extracts all email addresses from a given string:

```
import Foundation

let inputString = "My email is johnappleseed@example.com and my friend's email is janedoe@example.com"

do {
    let regex = try NSRegularExpression(pattern: "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,64}", options: .caseInsensitive)

    let matches = regex.matches(in: inputString, options: [], range: NSRange(location: 0, length: inputString.utf16.count))

    for match in matches {
        let email = (inputString as NSString).substring(with: match.range)
        print(email)
    }
} catch {
    print("Regex error: \(error.localizedDescription)")
}

// Output: johnappleseed@example.com
// Output: janedoe@example.com
```

In this example, we are searching for strings that match the pattern of an email address and extracting them from our input string. The `\` character is used for escaping characters that have special meaning in regular expressions. In this case, we are escaping the `.` character to ensure it is interpreted as a literal period. The `+` and `{}` symbols indicate that the preceding character must occur one or more times, and the `caseInsensitive` option allows for case-insensitive matching.

## Deep Dive

Regular expressions offer a wide range of flexibility with their patterns and options, allowing for efficient string manipulation and data extraction. It's essential to understand the various special characters and their meanings to fully utilize this tool. Here are some commonly used characters and their meanings:

- `.`: Matches any character except a line break.
- `+`: Matches one or more occurrences of the preceding character.
- `*`: Matches zero or more occurrences of the preceding character.
- `?`: Makes the preceding character optional (zero or one occurrence).
- `^`: Matches the beginning of a line.
- `$`: Matches the end of a line.
- `[]`: Matches any character within the brackets.
- `[^]`: Matches any character not within the brackets.
- `|`: Matches either the expression preceding or following the symbol.
- `()`: Creates a capture group for extracting specific parts of the matched string.

For more in-depth information about regular expressions, you can refer to the official documentation or other resources listed below.

## See Also

- [Official Swift Documentation on Regular Expressions](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Regexr - Online Regular Expression Tester](https://regexr.com/)
- [Regular Expressions in Swift by Hacking with Swift](https://www.hackingwithswift.com/articles/108/regular-expressions-in-swift)