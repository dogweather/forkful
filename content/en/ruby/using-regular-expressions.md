---
title:                "Using regular expressions"
html_title:           "Ruby recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions are a handy tool used by programmers to search for patterns within strings of text. They allow for flexible and powerful text matching, making it easier to find and extract specific information from large datasets. 

## How to:

### Basic Syntax
To use regular expressions in Ruby, we need to start by creating a regular expression object using the `Regexp.new` method or by using the `%r{}` notation. For example, to look for the string "hello" in a string named `str`, we could use either of the following:

```Ruby
regex = Regexp.new("hello")
regex = /hello/
```
Next, we can use the `=~` operator to match the regular expression against our string. It will return the index of the first match found or `nil` if no match is found. For example:

```Ruby
str = "Hello World!"
if str =~ /World/
  puts "Match found!"
else
  puts "No match found."
end
```
This code will output `Match found!` as `World` is present in the `str` string.

### Wildcards
Regular expressions make use of special characters to represent patterns. One of those characters is the `.` which matches any single character. For example:

```Ruby
str = "apple, orange, banana"
if str =~ /a./
  puts "Match found!"
else
  puts "No match found."
end
```

This code will output `Match found!` as the regular expression `/a./` will match any string that starts with "a" followed by any single character.

### Character Classes
Character classes allow us to specify a group of characters that the regular expression can match against. They are represented within square brackets `[]` and can include ranges or individual characters. For example:

```Ruby
str = "cat, rat, bat"
if str =~ /[cr]at/
  puts "Match found!"
else
  puts "No match found."
end
```

This code will output `Match found!` as the regular expression `/[cr]at/` will match any string that starts with "c" or "r" followed by "at".

## Deep Dive
Regular expressions were first introduced in the 1950s by mathematician Stephen Kleene for use in formal language theory. They were later adopted by many programming languages, including Ruby, for pattern matching and text manipulation.

While regular expressions are powerful, they can also be complex and difficult to read. As an alternative, Ruby provides String methods such as `include?`, `index`, and `gsub`, which can also be used for pattern matching in simpler cases.

It is also important to note that regular expressions in Ruby are case sensitive by default. To make them case insensitive, we can use the `i` option after the regular expression like this: `/hello/i`.

## See Also
- [Ruby's official documentation on regular expressions](https://ruby-doc.org/core-2.7.2/Regexp.html)
- [A beginners guide to regular expressions in Ruby](https://www.rexegg.com/regex-ruby.html)
- [Regular expressions in Ruby vs. other programming languages](https://www.codeschool.com/blog/2017/03/22/whats-the-difference-regex-ruby-python-php-javascript-golang/)

Happy coding! 🚀