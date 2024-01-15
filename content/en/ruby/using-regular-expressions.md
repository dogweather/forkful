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

## Why
Regular expressions, or regex, are a powerful tool in the world of programming. They allow you to search, match, and manipulate text in a precise and efficient manner. Engaging in using regular expressions can make your coding tasks easier and more effective.

## How To
To use regular expressions in Ruby, first you need to create a pattern using the `RegExp` class. For example, to match a string that starts with "Hello" and ends with "world", you would use `/^Hello.*world$/`. Let's see this in action with some code!

```Ruby
str = "Hello there, welcome to the world of Ruby"
if str =~ /^Hello.*world$/
  puts "You have entered the world of Ruby"
else
  puts "Sorry, this is not the world of Ruby"
end
```

The output of this code would be:

```
You have entered the world of Ruby
```

This is because the `=~` operator checks if the string matches the pattern and returns the index of the first match. If the string does not match the pattern, it returns `nil`.

## Deep Dive
Regular expressions can also be used for text manipulation, such as finding and replacing specific strings. Let's say we want to replace all instances of "world" with "universe" in the string "Hello world, how are you?". We can do this using the `gsub` method, which globally substitutes all matches with a new string.

```Ruby
str = "Hello world, how are you?"
new_str = str.gsub(/world/, "universe")
puts new_str
```

The output of this code would be:

```
Hello universe, how are you?
```

Regular expressions can be quite complex, but they offer a lot of flexibility for text processing tasks. You can use special characters like `?` to match zero or one instances of a character, `+` to match one or more, and `*` to match zero or more. It's important to note that Ruby's regular expressions are case sensitive, unless you specify otherwise.

## See Also
- [Ruby Documentation on Regular Expressions](https://ruby-doc.org/core-2.7.1/Regexp.html)
- [Regex Tutorial by Ruby Monstas](https://rubymonstas.org/blog/2014/10/18/ruby-for-beginners-regular-expressions.html)
- [Rubular: A Ruby Regular Expression Editor](https://rubular.com/)

Regular expressions can be a bit tricky at first, but with practice, they can become a valuable tool in your programming arsenal. Keep experimenting and learning, and see what incredible things you can do with regex!