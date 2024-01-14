---
title:                "Ruby recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions, or "regex" for short, are an essential tool in every programmer's arsenal. They allow you to search for patterns within text and perform powerful string manipulation operations. Whether you are a beginner or an experienced developer, understanding regular expressions can greatly enhance your programming skills.

## How To

To start using regular expressions in Ruby, we simply need to create a new regular expression object using the `//` notation. Let's say we want to search for all words that start with the letter "b" in a given string:

```Ruby
str = "Bob is a big boy"
regex = /b\w+/
matches = str.scan(regex)
puts matches
```

The above code will print out `["Bob", "big", "boy"]`. Let's break it down:

- The `b\w+` regex pattern means "match any word that starts with the letter b, followed by one or more word characters.
- The `scan` method on the `str` variable returns an array of all the matches found in the string.
- Finally, we use `puts` to print out the matches array to the console.

Regular expressions also allow us to perform substitutions. Let's say we want to replace every instance of "big" with "small" in our string:

```Ruby
str = "Bob is a big boy"
regex = /big/
new_string = str.sub(regex, "small")
puts new_string
```

The output will be `"Bob is a small boy"`. Here, we used the `sub` method to find the first match and replace it with our desired text.

## Deep Dive

Regular expressions have many more features and symbols that can be used to create complex patterns for matching and substitution. Some important ones to note are:

- `.` - matches any character except a newline
- `?` - makes the preceding token optional
- `+` - matches one or more of the preceding token
- `*` - matches zero or more of the preceding token
- `^` - matches the beginning of a string
- `$` - matches the end of a string
- `\A` - matches the beginning of a string (including newlines)
- `\z` - matches the end of a string (including newlines)
- `\b` - matches a word boundary
- `\d` - matches a digit (0-9)
- `\s` - matches any whitespace character
- `\w` - matches any word character (letter, number, underscore)

Regular expressions can also be used to validate string inputs, extract specific information from a string, and much more. It may seem daunting at first, but with practice and a good reference guide, you'll soon become a regex pro.

## See Also

- [IRB Tutorial: Using Regular Expressions in Ruby](https://ruby-doc.org/core/Regexp.html)
- [Regular Expressions Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)
- [Official Ruby Regular Expressions Documentation](https://ruby-doc.org/core-2.7.0/Regexp.html)
- [Ruby on Rails Guide: Regular Expressions](https://guides.rubyonrails.org/v3.2.21/active_support_core_extensions.html#regular-expressions)