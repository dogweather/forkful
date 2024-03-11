---
date: 2024-02-03 19:02:59.191386-07:00
description: "Regular expressions (regex) in Ruby are patterns used to match character\
  \ combinations in strings, enabling developers to search for, match, and manipulate\u2026"
lastmod: '2024-03-11T00:14:34.426634-06:00'
model: gpt-4-0125-preview
summary: "Regular expressions (regex) in Ruby are patterns used to match character\
  \ combinations in strings, enabling developers to search for, match, and manipulate\u2026"
title: Using regular expressions
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) in Ruby are patterns used to match character combinations in strings, enabling developers to search for, match, and manipulate text efficiently. Programmers utilize regex for tasks such as validation, parsing, and string manipulation, making it an indispensable tool for text processing.

## How to:
### Basic Matching
To match a string against a simple pattern, you can use the `match` method. Below, we're checking if the word "Ruby" exists in a given string.

```ruby
if /Ruby/.match("Hello, Ruby!")
  puts "Match found!"
end
# Output: Match found!
```

### Pattern Matching with Variables
You can interpolate variables into your regex using the `#{}` syntax, making your patterns dynamic.

```ruby
language = "Ruby"
if /#{language}/.match("Programming in Ruby is fun.")
  puts "Talking about Ruby!"
end
# Output: Talking about Ruby!
```

### Using Regex for Substitution
The `gsub` method allows you to replace every occurrence of a pattern with a specified replacement string.

```ruby
puts "foobarfoo".gsub(/foo/, "bar")
# Output: barbarbar
```

### Capturing
Parentheses in a regex are used for capturing parts of a match. The `match` method returns a `MatchData` object, which you can use to access captures.

```ruby
match_data = /(\w+): (\d+)/.match("Age: 30")
puts match_data[1] # Captured label
puts match_data[2] # Captured value
# Output:
# Age
# 30
```

### Using Third-Party Libraries
Although Ruby's standard library is powerful, you might sometimes need more specialized functionality. One popular gem for working with regex is `Oniguruma`, which provides additional regex features beyond the built-in Ruby regex engine.

Install it using:
```bash
gem install oniguruma
```

Example usage could look like this (assuming you have required `oniguruma` after installing it):

```ruby
# This is a more advanced example and might require additional setup
require 'oniguruma'

pattern = Oniguruma::ORegexp.new('(\d+)')
match_data = pattern.match("The number is 42.")
puts match_data[1]
# Output: 42
```

Remember, while powerful, regular expressions can become complex and hard to manage for more complicated patterns. Aim for readability, and consider alternative methods if your regex becomes too convoluted.
