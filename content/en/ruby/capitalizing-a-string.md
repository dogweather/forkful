---
title:                "Capitalizing a string"
html_title:           "Ruby recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing a string is a common task in programming, whether you're working on a web application, data analysis, or simple text manipulation. By capitalizing a string, you can easily transform text to follow specific formatting rules or match certain criteria. Plus, it's just good practice to have consistent capitalization in your code.

## How To

The process of capitalizing a string in Ruby is straightforward and can be achieved in multiple ways. Here are three commonly used methods:

1. Using the `capitalize` method: This method capitalizes the first character of a given string, leaving the rest of the string unchanged.

```Ruby
string = "hello world"
puts string.capitalize
# Output: Hello world
```

2. Using the `upcase` method: This method capitalizes all characters in a string, essentially transforming the string into all uppercase letters.

```Ruby
string = "hello world"
puts string.upcase
# Output: HELLO WORLD
```

3. Using string interpolation and calling `capitalize` on a specific part of the string: This method allows you to capitalize specific letters or words within a string by using string interpolation. Here's an example of capitalizing the first letter of a string using this method:

```Ruby
string = "hello world"
puts "The first letter in your string is: #{string[0].capitalize}"
# Output: The first letter in your string is: H
```

## Deep Dive

The `capitalize` and `upcase` methods may seem similar, but they have an important difference. The `capitalize` method only capitalizes the first character of a string, while the `upcase` method capitalizes all letters in a string.

Additionally, the string interpolation method allows for more flexibility in capitalization since you can choose which parts of the string to capitalize.

It's also important to note that both `capitalize` and `upcase` do not modify the original string, but instead return a copy of the string with the desired capitalization. If you want the original string to be modified, you can add a `!` to the end of the method, like `capitalize!` or `upcase!`.

## See Also

- [Ruby String Documentation](https://ruby-doc.org/core-3.0.1/String.html)
- [Ruby String Methods](https://www.rubyguides.com/ruby-string-methods/)
- [String Interpolation in Ruby](https://thoughtbot.com/blog/just-f-un-with-ruby-string-to_proc)