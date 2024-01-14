---
title:                "Swift recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

Why

Have you ever needed to pull out a specific part of a string in your Swift code? Maybe you want to extract a name from a longer sentence or break apart a string based on certain characters. This is where extracting substrings comes in handy.

How To

To begin, we will start with a simple string: 

```Swift
let sentence = "Hello, my name is John."
```

To extract a substring from this string, we can use the `substring` method. First, we need to determine the range of characters we want to extract. For example, let's say we want to extract "John" from the string. We can do this by creating a `Range` object using the `..<` operator.

```Swift
let range = ...<15
```

Next, we can use the `substring` method on our string variable, passing in the range we just created. Note that the range goes from the first character we want to extract up to, but not including, the character at the end of the range.

```Swift
let name = sentence.substring(with: range)
```

Now, if we print out `name`, we will see the desired output of "John".

```
John
```

We can also use the `substring` method to extract multiple substrings from a string. For example, let's say we want to extract both "Hello" and "John" from our original string. We can do this by creating a range that specifies the start and end of each substring.

```Swift
let range1 = ...<5     // for "Hello"
let range2 = 21..<26  // for "John"
```

Then, we can simply call the `substring` method twice, passing in the appropriate range for each substring.

```Swift
let greeting = sentence.substring(with: range1)
let name = sentence.substring(with: range2)
```

This will give us the following output:

```
Hello
John
```

Deep Dive

The `substring` method is actually a shorthand for accessing a `substring(from:)` method and a `substring(to:)` method individually. The `substring(from:)` method takes in an integer that specifies the starting index of the substring, while the `substring(to:)` method takes in an integer that specifies the ending index.

Another important thing to note is that the `substring` method returns a `String` type, not a `Substring` type. If you want to work with `Substring` types instead, you can use the `prefix` and `suffix` methods.

See Also

For more information on manipulating strings in Swift, check out the official documentation here: https://developer.apple.com/documentation/swift/string