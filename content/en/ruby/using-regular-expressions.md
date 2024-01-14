---
title:    "Ruby recipe: Using regular expressions"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why

Regular expressions are a powerful tool in the world of programming. They allow us to efficiently search for and manipulate text in a wide variety of ways. Whether you are a beginner or an experienced programmer, learning and mastering regex can greatly improve your coding skills.

## How To

To use regular expressions in Ruby, we first need to require the `re` module. This module contains all the methods and classes we need to work with regex.

```Ruby
require 're'
```

Next, we can create a new regex object by using the `Regexp.new` method and passing in our desired expression as a string.

```Ruby
regex = Regexp.new("hello")
```

We can then use this object to perform various tasks, such as searching for a pattern in a string. Let's try searching in the string "Hello World" using the `match` method.

```Ruby
"Hello World".match(regex)
# => #<MatchData "Hello">
```

As we can see, the `match` method returns a `MatchData` object which contains information about the match, such as the matched string and its position in the original string.

We can also use regex to perform substitutions, using the `sub` method. Let's replace all instances of "Python" with "Ruby" in the string "I love coding in Python".

```Ruby
"I love coding in Python".sub(/Python/, "Ruby")
# => "I love coding in Ruby"
```

These are just a few examples of how we can use regular expressions in Ruby. Experiment with different expressions and methods to discover their full potential!

## Deep Dive

Regular expressions follow a specific pattern syntax, with special characters and metacharacters that have specific meanings. For example, the `.` character represents any single character, while the `*` character represents any number of the previous character or group.

They can also be modified with various modifiers, such as the `i` modifier for case-insensitive matching, or the `m` modifier for multiline matching.

To get a better understanding of regex and its syntax, it's important to read through the Ruby documentation and practice writing your own expressions.

## See Also

- [Ruby Documentation on Regular Expressions](https://ruby-doc.org/core-3.0.0/Regexp.html)
- [Rubular](https://rubular.com/) - a handy online tool for testing and visualizing regex
- [Mastering Regular Expressions, 3rd Edition](https://www.oreilly.com/library/view/mastering-regular-expressions/9780596528126/) - a comprehensive book on regex and its applications