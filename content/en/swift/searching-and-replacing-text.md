---
title:                "Searching and replacing text"
html_title:           "Swift recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? 
Searching and replacing text in programming refers to finding specific words or patterns in a section of code and replacing them with new ones. This is a common task for programmers when making changes to their code or fixing errors. It allows for quick and efficient updates to large amounts of text without having to manually go through each line. 

## How to:
To search and replace text in Swift, we can use the `replacingOccurrences(of:with:)` method. Let's take a look at an example:

```
let sentence = "Hello World!"
let newSentence = sentence.replacingOccurrences(of: "World", with: "Universe")
print(newSentence)
```

This will output: `Hello Universe!`. As you can see, by using this method, we were able to easily replace the word "World" with "Universe" in our string. 

We can also use the method `replacingOccurrences(of:with:options:range:)` to specify a range in which we want the replacement to occur. For example: 

```
let sentence = "Hello Swift!"
let newSentence = sentence.replacingOccurrences(of: "Swift", with: "World", options: [], range: nil)
print(newSentence)
```

This will output: `Hello World!` as we replaced only in the specified range. 

## Deep Dive:
Searching and replacing text has been a common practice for programmers since the early days of coding. Before the use of string manipulation methods, programmers had to manually find and replace text in their code. However, with the advancement of programming languages and tools, this task has become much easier and quicker to accomplish. 

Alternatives to using the `replacingOccurrences` method include using the more complex regular expressions and the `String`'s `replacingMatches` method. These methods offer more flexibility and precision in searching and replacing text, but may require more experience and knowledge to use effectively. 

It's important to note that when using the `replacingOccurrences` method, the original string remains unchanged and a new string with the replacements is returned. This makes it a safer option when making changes to important sections of code. 

## See Also:
To learn more about searching and replacing text in Swift, check out the official [documentation](https://developer.apple.com/documentation/foundation/nsstring/1417167-replacingoccurrences) from Apple. You can also explore other methods and techniques for string manipulation in Swift to enhance your coding skills.