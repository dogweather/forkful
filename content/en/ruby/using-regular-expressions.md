---
title:                "Ruby recipe: Using regular expressions"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are a powerful tool in the world of programming. They allow us to search and manipulate text in a flexible and efficient manner. Learning how to use regular expressions can save you time and effort by automating tasks that would otherwise be tedious to do manually. If you want to become a more efficient programmer, regular expressions are a skill worth mastering.

## How To

To use regular expressions in Ruby, we first need to create a regular expression object using the `Regexp` class. This object contains the pattern we want to match within a string. We can create a Regexp object using `/.../` or `%r{...}` delimiters. For example:

```Ruby
/\d{3}-\d{3}-\d{4}/  # matches a phone number in the format of XXX-XXX-XXXX
%r{^\w+@\w+\.(com|net|org)}  # matches an email address
```

Once we have our Regexp object, we can use it with the `match` method to find matches within a string. For example:

```Ruby
phone_number = "555-123-4567"
phone_number.match(/\d{3}-\d{3}-\d{4}/) # returns a MatchData object
```

We can also use regular expressions with the `sub` and `gsub` methods to replace text in a string. For example:

```Ruby
text = "Hello, my name is John."
text.gsub(/John/, "Jane") # returns "Hello, my name is Jane."
```

## Deep Dive

Regular expressions can get quite complex, but there are some common symbols and shortcuts that you should be familiar with:

- `.` matches any single character
- `[]` matches any character within the brackets
- `+` matches one or more occurrences of the previous character or group
- `*` matches zero or more occurrences of the previous character or group
- `\` is used to escape special characters, such as `.` or `[]`

It's important to note that regular expressions are case-sensitive by default, but you can use the `i` modifier to make them case-insensitive. Additionally, the `m` modifier can be used to make a regular expression match across multiple lines.

Be sure to also check out the Ruby documentation for more information and examples on using regular expressions.

## See Also

- [Ruby Regular Expressions](https://ruby-doc.org/core-2.7.1/Regexp.html)
- [Rubular](https://rubular.com/) - a handy online tool for testing regular expressions
- [Regular Expression Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/ruby) - a quick reference guide for common regular expression symbols and shortcuts.