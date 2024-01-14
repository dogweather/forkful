---
title:    "Swift recipe: Capitalizing a string"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Why
Have you ever needed to capitalize a string in your Swift code? Maybe you're creating an app that requires proper capitalization for user inputs, or you just want to format some text in a specific way. Whatever the reason may be, capitalizing a string is a simple and useful technique to have in your programming toolkit.

## How To
To capitalize a string in Swift, we can use the built-in `uppercased()` method. Let's see an example:

```Swift
let stringToCapitalize = "hello world"
let capitalizedString = stringToCapitalize.uppercased()

print(capitalizedString)
```

This will print out `HELLO WORLD`, showing that the `uppercased()` method has successfully capitalized the string and returned a new string with the desired format.

We can also use `capitalize()` to only capitalize the first letter of the string, while leaving the rest of the letters in their original lowercase form. For example:

```Swift
let stringToCapitalize = "hello world"
let capitalizedString = stringToCapitalize.capitalize()

print(capitalizedString)
```

This will print out `Hello world`, with only the "H" in "Hello" being capitalized.

One thing to note is that both `uppercased()` and `capitalize()` return a string, so if the desired result is to directly change the original string to be capitalized, we can use the `mutating` keyword before the method. For example:

```Swift
var stringToCapitalize = "hello world"
stringToCapitalize.uppercased()
print(stringToCapitalize) // Output: hello world

stringToCapitalize.uppercased()
print(stringToCapitalize) // Output: HELLO WORLD
```

Using the `mutating` keyword changes the original string, instead of just returning a new one.

## Deep Dive
Although it may seem like a simple task, capitalizing a string involves more than just changing the letters to uppercase. In Swift, strings are represented by the `String` type, which is actually a collection of `Character` values. This means that when we use the `uppercased()` method, we are actually iterating through each character in the string and changing it to uppercase. This is important to keep in mind, as it can impact the efficiency and performance of our code.

Another thing to consider is how the method handles special characters and non-English letters. For example, the German letter "ß" cannot be properly capitalized, and will be changed to "SS" instead. This is because uppercase letters in German are represented as "A, B, C, ..." while lowercase letters are "a, b, c, ..." and "ß" does not have an uppercase counterpart. Therefore, it is important to be aware of these nuances when using the `uppercased()` method.

## See Also
- Official Swift documentation for `uppercased()`: https://developer.apple.com/documentation/swift/string/2995497-uppercased
- Official Swift documentation for `capitalize()`: https://developer.apple.com/documentation/swift/string/2995495-capitalize
- Ray Wenderlich tutorial on string manipulation in Swift: https://www.raywenderlich.com/1260-swift-string-cheat-sheet-and-reference