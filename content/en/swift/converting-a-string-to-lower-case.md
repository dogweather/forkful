---
title:    "Swift recipe: Converting a string to lower case"
keywords: ["Swift"]
---

{{< edit_this_page >}}

##Why

In Swift programming, converting a string to lower case can be a useful tool when dealing with user inputs or comparing strings. By converting strings to lower case, you can ensure that the case of the letters does not affect the functionality of your code. Let's explore how to do this in Swift!

##How To

Converting a string to lower case in Swift is simple and can be done using the `lowercased()` method. Let's take a look at an example:

```Swift
let string = "Hello World"
let lowercasedString = string.lowercased()
print(lowercasedString) //output: hello world
```

As you can see, the `lowercased()` method is applied to the string and its output is assigned to a new variable. The result is a lowercased version of the original string.

You can also use this method on user inputs, which can be helpful when comparing strings. Let's say we want to check if the user's input matches a secret word, but we want the comparison to be case-insensitive. We can use `lowercased()` on both strings to ensure that the comparison is accurate regardless of the case of the letters.

```Swift
let secretWord = "password"
let userInput = "PASSWORD"
if userInput.lowercased() == secretWord.lowercased() {
  print("Access granted!")
} else {
  print("Access denied.")
}
//output: Access granted!
```

##Deep Dive

Behind the scenes, the `lowercased()` method uses the Unicode standard to convert all of the letters in the string to lower case. This ensures that the method works correctly for all languages and special characters. It's also worth noting that this method does not change the original string, but instead returns a new lowercased string.

##See Also

To learn more about string manipulation in Swift, check out these resources:

- [String Manipulation in Swift](https://www.hackingwithswift.com/articles/131/string-manipulation-in-swift)
- [String Manipulation Techniques in Swift](https://medium.com/swift-india/string-manipulation-techniques-in-swift-5ee5ed0f636b)
- [The Swift Programming Language: Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)