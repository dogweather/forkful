---
title:                "Ruby recipe: Converting a string to lower case"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why 

Converting strings to lower case is an essential task in any programming language, including Ruby. By converting a string to lower case, we can manipulate and compare text in a case-insensitive manner. This is useful for tasks such as user input validation, sorting and searching data, and formatting text for display. In this blog post, we'll explore different ways to convert strings to lower case in Ruby.

## How To

To convert a string to lower case in Ruby, we can use the `downcase` method. Let's take a look at an example:

```Ruby
string = "HELLO WORLD"
puts string.downcase
```

This will output `hello world`, all in lowercase letters. We can also use the `downcase!` method to modify the original string instead of creating a new one. This is useful if we want to permanently change the string.

```Ruby
string = "HELLO WORLD"
string.downcase!
puts string
```

This will output `hello world` since the `downcase!` method modifies the string in place. We can also use the `downcase` method on individual characters within a string using the `each_char` method:

```Ruby
string = "HeLLO wORld"
string.each_char do |char|
  print char.downcase
end

# Output: hello world
```

## Deep Dive

Ruby's `downcase` method uses the Unicode character mapping for case conversions. This means that it can handle special characters from different languages, making it a powerful tool for internationalization. Additionally, the `downcase` method does not modify the original string but returns a new string with the case conversion. The `downcase!` method, on the other hand, directly modifies the original string.

We can also use the `downcase` method in conjunction with regular expressions for more complex string manipulation. For example, we can use the `gsub` method to replace all uppercase letters with lower case ones, while preserving any other characters:

```Ruby
string = "HeLLO wORld!"
string = string.gsub(/[A-Z]+/) { |match| match.downcase }
puts string

# Output: hello world!
```

## See Also

- [Ruby's `String` class documentation](https://ruby-doc.org/core-2.7.1/String.html)
- [Ruby's `String#downcase` method documentation](https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase)
- [Ruby's `gsub` method documentation](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)