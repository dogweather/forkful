---
title:                "Converting a string to lower case"
html_title:           "Ruby recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# What & Why?

Converting a string to lower case means transforming all the letters in a string to their lowercase equivalent. Programmers do this to standardize data and make it easier to compare strings. It also allows for better data validation and user input handling.

# How to:

```Ruby
# 1. Using the downcase method
string = "Hello World"
puts string.downcase # output: hello world

# 2. Using the lowercase modifier (only supported in Ruby 2.4+)
string = "Hello World"
puts string.downcase # output: hello world
```

# Deep Dive

Converting strings to lower case has been a common practice since the early days of programming. In the early programming languages, there was no built-in function for this task, so programmers had to use complex algorithms to achieve the same result. As programming languages evolved, the downcase method was introduced to make this conversion easier and more efficient.

In Ruby, the downcase method is the standard way of converting strings to lower case. However, there is a newer method, lowercase, which was introduced in Ruby 2.4. This method directly modifies the original string, rather than returning a new one like the downcase method.

The downcase method and lowercase modifier can also be used with non-alphabetic characters, but they will remain unchanged. For example, "123abc" will be converted to "123abc" as all numbers and symbols are ignored.

# See Also

- [Ruby Documentation on String#downcase](https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase)
- [Ruby Documentation on String#lowercase](https://ruby-doc.org/core-2.7.1/String.html#method-i-lowercase)
- [Stack Overflow Article on String Conversions in Ruby](https://stackoverflow.com/questions/1387002/how-to-convert-a-string-to-lower-or-upper-case-in-ruby)