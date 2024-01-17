---
title:                "Finding the length of a string"
html_title:           "Ruby recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string means determining the number of characters in a given string. This is a commonly used operation in programming because it allows us to manipulate strings and perform various tasks on them.

## How to:

```Ruby
string = "Hello, World!"
puts string.length
```

Output: `13`

We can use the `length` method on a string variable to find out its length. This method returns an integer representing the number of characters in the string. In the above example, the string "Hello, World!" has a length of 13.

## Deep Dive

### Historical Context:

In the early days of computing, strings were limited to a fixed length. But with the development of high-level programming languages like Ruby, strings now have a variable length and can be as long as the system's memory allows. The length of a string is now a dynamic property that can change as the string is modified.

### Alternatives:

Apart from using the `length` method, we can also find the length of a string by converting it into an array and then using the `count` method. This approach is useful when we want to count specific characters or substrings within a string.

### Implementation Details:

The `length` method calculates the number of bytes required to store the string, which may vary depending on the character encoding used. This can cause discrepancies in the length of a string if different character encodings are used.

## See Also:

- [Ruby String Documentation](https://ruby-doc.org/core-#{version}/String.html)
- [Ruby Array Documentation](https://ruby-doc.org/core-#{version}/Array.html)