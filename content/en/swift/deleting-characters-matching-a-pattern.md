---
title:                "Swift recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

##Why
"Deleting characters matching a pattern" is a common task in Swift programming, especially when dealing with user input or text manipulation. Knowing how to efficiently delete these characters can save you time and frustration in your coding process.

##How To
To delete characters matching a pattern, we can use the `replacingOccurrences(of:with:options:range:)` method available in the `NSString` class. It takes in 4 parameters: the pattern to be matched, the replacement string, options for the matching behavior, and the range of text to search within. Let's look at an example:

//Assuming we have the string "Hello, world!" and we want to delete all the vowels
let str = "Hello, world!"

//We use the `replacingOccurrences` method and specify "aeiou" as the pattern to be matched
let newStr = str.replacingOccurrences(of: "aeiou", with: "", options: [.caseInsensitive], range: nil)

//Our output will be "Hll, wrld!" since all the vowels have been deleted

It's important to note that the `options` parameter allows us to specify any additional matching options, such as ignoring case sensitivity. The `range` parameter is optional and can be used to specify a specific range of text to search within.

##Deep Dive
Behind the scenes, the `replacingOccurrences` method is actually using regular expressions to match the given pattern. This gives us a lot of flexibility in choosing the characters to be deleted. For example, we can use the `CharacterSet` class to specify a set of characters to be matched, instead of providing a specific string of characters. This is useful when dealing with user input that may vary in formatting.

Another important aspect to consider is the performance of using regex. While it is a powerful tool, it can also be computationally expensive. Thus, it's important to test and optimize your code for efficient execution.

##See Also
- Apple Documentation on `replacingOccurrences(of:with:options:range:)`: https://developer.apple.com/documentation/foundation/nsstring/1411675-replacingoccurrences
- A Beginner's Guide to Regular Expressions in Swift: https://www.raywenderlich.com/5769044-regular-expressions-tutorial-getting-started
- Improve Your Swift Code Using NSRegularExpression: https://medium.com/pretty-swifty/improve-your-swift-code-using-nsregularexpression-bea619c422d3