---
title:                "Capitalizing a string"
html_title:           "Swift recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string means converting the first character of each word in the text, to uppercase. It enhances readability, makes text visually appealing, and enables programmers to establish text standards for their projects.

## How to:
In Swift, use `capitalized` property of the string to change its first character to upper case. Remember, it only affects the first letter of a word, and leaves the rest of the string as it is.

```Swift
let myString = "hello world"
let capitalizedString = myString.capitalized
print(capitalizedString)   //"Hello World"
```
The above code will output: "Hello World"

## Deep Dive
Historically, capitalizing strings helped typographers distinguish elements in documents. Swift's `capitalized` follows Unicode standards for capitalization, considering language-specific rules. 

An alternative way to capitalize a string is:

```Swift
myString.uppercased()
```
This will turn all characters to uppercase, however. If your use case requires only first-letter capitalization, stick with `capitalized`.

Remember, `capitalized` is a computed property, not a method. It returns a new string each call. When using it extensively, consider its effect on performance.

## See Also
Capitalize strings with special rules? Check out `uppercased(with: Locale?)`:
- [Apple Documentation](https://developer.apple.com/documentation/swift/string/2294250-uppercased)

Expecting a string already in memory when capitalizing? Dive into:
- [Swift's copy on write.](https://developer.apple.com/videos/play/wwdc2015/414/)

Please note, `capitalized`, `lowercased()` and `uppercased()` are part of `Foundation`. List of String transformations:
- [Apple Documentation](https://developer.apple.com/documentation/foundation/stringtransform)