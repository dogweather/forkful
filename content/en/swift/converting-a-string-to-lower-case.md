---
title:                "Swift recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Why
As a Swift programmer, you may come across a situation where you need to convert a string to lower case. This is a common task in string manipulation and can be useful for various purposes such as data validation or formatting.

# How To
To convert a string to lower case in Swift, you can use the `lowercased()` method. Let's take a look at a simple example:

```Swift 
let name = "JOHN DOE"
let lowercasedName = name.lowercased()
print(lowercasedName)
// Output: john doe
```

In this code, we create a string variable named `name` and assign it the value "JOHN DOE". Then, we use the `lowercased()` method to convert the string to lower case and assign it to a new variable `lowercasedName`. Finally, we print the value of `lowercasedName` and the output is "john doe".

You can also use this method on string literals, making it easy to convert a string to lower case without the need for an extra variable:

```Swift
print("HELLO WORLD".lowercased())
// Output: hello world
```

# Deep Dive
There are a few things to keep in mind when using the `lowercased()` method. First, it is important to note that it is a Swift standard library method and is not available in Objective-C. This means that if you are working on a project that involves both Swift and Objective-C, you will need to find an alternative method for converting strings to lower case in Objective-C.

Another important thing to note is that the `lowercased()` method uses the default locale for case mapping. This means that if you are working with strings in different languages, the results may vary. For example, the German word "Strasse" would be converted to "strasse", but in Turkish, it would be converted to "straße". In cases where you need more control over the case conversion, you can use the `localizedLowercase` property which allows you to specify a specific locale.

# See Also
- [Apple's documentation for `lowercased()` method](https://developer.apple.com/documentation/swift/string/2894565-lowercased)
- [Stack Overflow thread on converting strings to lower case in Swift](https://stackoverflow.com/questions/24139293/converting-string-to-lowercase-in-swift)
- [Tutorial on working with strings in Swift](https://www.appcoda.com/swift-string-tutorial/)

*Note: This blog post was originally written in Swift 4.2 and some syntax may have changed in later versions. It is always a good idea to refer to the official documentation for the most up-to-date information.*