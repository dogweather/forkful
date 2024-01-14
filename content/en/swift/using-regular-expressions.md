---
title:                "Swift recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why
Regular expressions are a powerful tool in any programmer's arsenal. They allow us to easily search, match, and manipulate text based on specific patterns. Whether you're working on a simple project or a complex one, regular expressions can save you time and effort by automating text processing tasks. In this blog post, we'll dive into the world of regular expressions and explore how they can enhance your Swift programming skills.

## How To

To use regular expressions in Swift, we'll need to import the `Foundation` framework which includes the `NSRegularExpression` class. Let's take a look at a simple example of how to use regular expressions to match a phone number pattern.

```Swift
import Foundation

let phoneNumber = "123-456-7890"

do {
    let regex = try NSRegularExpression(pattern: "^\\d{3}-\\d{3}-\\d{4}$", options: .anchorsMatchLines)
    let matches = regex.matches(in: phoneNumber, options: [], range: NSRange(location: 0, length: phoneNumber.utf16.count))
    
    if matches.count > 0 {
        print("Valid phone number!")
    } else {
        print("Invalid phone number.")
    }
} catch let error {
    print("Invalid regex: \(error.localizedDescription)")
}
```

Here, we use the `NSRegularExpression` class to define a pattern that consists of three sets of three digits separated by hyphens. The `^` and `$` symbols represent the beginning and end of the string, respectively. We also specify the `anchorsMatchLines` option to limit the match to the beginning and end of the string. Running this code will print "Valid phone number!" since the given string matches our regex pattern.

But how do we extract the actual phone number from the string? For that, we can use capture groups, which are defined by parentheses in the regex pattern. Let's modify our code to extract the phone number from a string that contains not only the number but also the name of the owner.

```Swift
let text = "John's phone number is 123-456-7890"

do {
    let regex = try NSRegularExpression(pattern: "([A-Za-z]+)'s phone number is (\\d{3}-\\d{3}-\\d{4})", options: .caseInsensitive)
    let matches = regex.matches(in: text, options: [], range: NSRange(location: 0, length: text.utf16.count))
    
    if let match = matches.first {
        let nameRange = match.range(at: 1)
        let numberRange = match.range(at: 2)
        
        if let name = text.substring(with: nameRange), let number = text.substring(with: numberRange) {
            print("\(name) has the phone number \(number)")
        }
    }
} catch let error {
    print("Invalid regex: \(error.localizedDescription)")
}
```

In this example, we use `range(at:)` to retrieve the ranges of the capture groups, and then use `substring(with:)` to extract the matched strings. We can also specify the case-insensitive option to be able to match names in different cases. Running this code will print "John has the phone number 123-456-7890."

## Deep Dive

Regular expressions can be used for much more than just string matching. They also have the power to replace, split, and even validate complex patterns. The regex syntax may seem intimidating at first, but with some practice and the help of online resources such as [Regex101](https://regex101.com/) and [Regexr](https://regexr.com/), you'll be able to master it in no time.

Some helpful tips when using regular expressions in Swift:

- Use `try?` when creating the `NSRegularExpression` object to handle any potential errors.
- Use the `options` parameter in `matches(in:options:range:)` to specify additional matching behaviors, such as ignoring case sensitivity.
- Use capture groups and named capture groups to extract specific parts of a matched string.
- Use `replaceMatches(in:options:range:withTemplate:)` to replace text based on a regex pattern.
- Use `components(separatedBy:)` with a regex pattern to split a string into an array.
- Experiment with different regex patterns to find the most efficient one for your particular use case.

## See Also
- [NSRegularExpression Class Reference](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Regex101](https://regex101.com/)
- [Regexr](https://regexr.com/)