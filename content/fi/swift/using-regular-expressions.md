---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Swift: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Finnish readers may wonder why they should bother learning about regular expressions when there are simpler ways to manipulate strings in Swift. The answer is that regular expressions offer a more powerful and efficient way to search and manipulate text data, especially when dealing with more complex patterns.

## Miten

Regular expressions in Swift are denoted by using the "let regex = try! Regex(pattern:)" syntax. The pattern is written between the quotation marks, and there are various symbols and characters that can be used to define the pattern. For example, the dot (.) represents any single character, while the asterisk (*) represents zero or more occurrences of the preceding character.

```Swift
let regex = try! Regex(pattern: "c[ae]t")
if regex.matches("cat") {
  print("Match found!")
}
// Output: Match found!
```

In the above code, the regular expression "c[ae]t" will match both "cat" and "cet". The square brackets indicate a set of characters that can be used in the match, while the asterisk after the closing bracket means that the preceding character (in this case "a" or "e") can occur zero or more times.

## Syväsukellus

Regular expressions can also be used for more complex pattern matching, such as using quantifiers to specify the number of occurrences of a character. For instance, the plus sign (+) means one or more occurrences, while the question mark (?) means zero or one occurrence.

```Swift
let regex = try! Regex(pattern: "S[ai]+t")
if regex.matches("Sit") {
  print("Match found!")
}
// Output: Match found!
```

The regular expression "S[ai]+t" will match "Sit" but not "St" because the pattern requires at least one occurrence of either "a" or "i" between "S" and "t".

Additionally, characters can be grouped together using parentheses and then referenced later in the string using back-references (\1, \2, etc.). This allows for more specific and customizable pattern matching.

## Katso myös

To learn more about regular expressions in Swift, check out the official Apple documentation and Regex101, a useful tool for testing and building regular expressions.