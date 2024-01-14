---
title:    "Swift recipe: Converting a string to lower case"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

In Swift programming, converting a string to lower case can be a useful task when dealing with user input or comparing strings. It allows for easier manipulation and comparison of strings without having to worry about case sensitivity.

## How To

Converting a string to lower case in Swift is a simple process. First, we declare a string variable with our desired phrase:

```
Swift let phrase = "HeLlO wOrLd"
```

Next, we use the `lowercased()` method to convert the string to lower case:

```
Swift let lowercasedPhrase = phrase.lowercased()
```

And that's it! We now have a new string variable with our phrase in all lower case:

```
Swift print(lowercasedPhrase)
// output: hello world
```

We can also use this method to directly convert user input to lower case, making it easier to compare with other strings:

```
Swift let userInput = "AppLe"
let lowercasedInput = userInput.lowercased()
if lowercasedInput == "apple" {
    print("You entered the fruit!")
}
// output: You entered the fruit!
```

## Deep Dive

Behind the scenes, the `lowercased()` method uses the Unicode Standard to convert characters to their lower case counterparts. This means that it can handle any language or special characters. It also takes into account language-specific rules for conversion.

It's important to note that this method does not modify the original string. Instead, it returns a new string with the converted characters. This is because strings in Swift are immutable, meaning they cannot be changed after they are created. So, if you need to use the converted string multiple times, be sure to save it as a new variable.

## See Also

For more information on string manipulation in Swift, check out these helpful resources:

- [Swift Strings and Characters - Apple Developer](https://developer.apple.com/documentation/swift/string)
- [The Swift Programming Language: Strings and Characters - Swift.org](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift String Cheat Sheet - raywenderlich.com](https://www.raywenderlich.com/287-string-cheat-sheet-for-swift-4-0)

Happy coding!