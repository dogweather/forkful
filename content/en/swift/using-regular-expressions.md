---
title:    "Swift recipe: Using regular expressions"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Why

Regular expressions are a powerful tool in every programmer's toolkit. This text-based pattern matching technique allows for efficient and flexible searching, replacing, and validation of strings. By mastering the use of regular expressions, developers can save time and effort in their projects, as well as improve the accuracy of their code.

## How To

To use regular expressions in Swift, first import the `Foundation` framework and then use the `NSRegularExpression` class. Let's say we want to extract all numbers from a string. We can achieve this with a regular expression pattern by using the `matches(in:options:range:)` method and passing in the appropriate parameters:

```Swift
import Foundation

let string = "I have 5 apples, 2 oranges, and 3 bananas."
let pattern = #"\d+"# // this represents any digit
let regex = try! NSRegularExpression(pattern: pattern)
let matches = regex.matches(in: string, options: [], range: NSRange(location: 0, length: string.utf16.count))

for match in matches {
    print(String(string[Range(match.range, in: string)!]))
}

// Output:
// 5
// 2
// 3
```

In the above example, we used the `matches(in:options:range:)` method to find all occurrences of the regular expression pattern in the given string. We then loop through the matches and print the extracted numbers. The `NSRange` and `Range` classes are used to specify the range within the string where the regular expression should be applied.

You can also use regular expressions for string replacement. Let's update our previous example to replace all numbers with the word "number":

```Swift
for match in matches {
    let range = Range(match.range, in: string)!
    string.replaceSubrange(range, with: "number")
}

print(string)

// Output:
// I have number apples, number oranges, and number bananas.
```

## Deep Dive

Regular expressions can be complex and have their own syntax, but the basics are not difficult to grasp. The `#"` and `"#` delimiters are used to mark the start and end of a regular expression pattern. Inside these delimiters, we can use special characters and sequences to describe the desired search pattern. For instance, `d` is a special character that represents any digit. Other commonly used special characters include `w` (any word character), `s` (any whitespace character), and `.` (any character).

In addition to special characters, we can use quantifiers to specify the number of repetitions of a pattern. For example, `*` means zero or more, `+` means one or more, and `?` means zero or one. We can also use parentheses to group patterns and use the `|` symbol to indicate alternatives.

Regular expressions can be used for more complex tasks such as email validation, URL parsing, and data extraction. With practice, developers can become fluent in using this powerful tool and greatly enhance their coding skills.

## See Also

Here are some helpful resources for learning more about regular expressions in Swift:

- [NSRegularExpression Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Regex in Swift: Complete Tutorial with Examples](https://www.appcoda.com/swift-regular-expressions/)
- [Regular Expressions Cheat Sheet](https://regexr.com)

Now that you have an understanding of the basics, it's time to start incorporating regular expressions into your Swift projects. Happy coding!