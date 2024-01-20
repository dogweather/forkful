---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

# String Interpolation in Swift
## What & Why?
String interpolation is a method to construct new strings that contain places to be substituted with values from various variables. Developers use this technique for efficient and readable code.

## How to:
In Swift, you can interpolate strings with the `\()` syntax. For instance:

```Swift
let name = "John"
let greeting = "Hello, \(name)"
print(greeting)
```
This will output:
```
Hello, John
```

Let's add more variables to this:

```Swift
let age = 28
let intro = "My name is \(name) and I'm \(age) years old."
print(intro)
```
And the output will be:
```
My name is John and I'm 28 years old.
```

## Deep Dive
Historically, developers used concatenation to combine strings, which often led to messy and unreadable code. Swift introduced string interpolation to simplify string manipulation and improve readability.

There are alternatives to string interpolation. You could use concatenation or format strings by using the `String(format:)` method:

```Swift
let greeting = "Hello, " + name
```
However, these methods lack the readability and simplicity that string interpolation provides.

Swift's string interpolation is incredibly powerful. It doesn't just substitute variable values, it can also evaluate expressions. For instance, `\(2 + 2)` will be replaced by `4` in your string.

## See Also
Further reading and tutorials:
- Swift docs on [Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Swift Programming Guide's section on [String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)