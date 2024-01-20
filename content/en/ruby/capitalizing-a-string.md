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

## What & Why?
String capitalization is the conversion of the first character in a string to uppercase. It's used in programming to sanitize data, ensure grammatical correctness, and enhance readability.

## How to:
In Ruby, the `capitalize` method is used to capitalize a string. Below is a simple illustration:

```Ruby
str = "hello world!" 
puts str.capitalize
# Prints: "Hello world!"  
```

It's important to note that `capitalize` only changes the first character of the string to uppercase. If you want to capitalize each word in the string, use `titleize`:

```Ruby
require 'active_support/core_ext/string'

str = "hello world!" 
puts str.titleize
# Prints: "Hello World!" 
```

## Deep Dive
The `capitalize` method has been part of Ruby since its initial release in 1995. On the other hand, `titleize` is an added feature from the ActiveSupport library, commonly used in Ruby on Rails.

There's a third way to capitalize strings: the `upcase` method to change all characters to uppercase. However, this isn't typically the preferred method for string capitalization because it's more aggressive than `capitalize` or `titleize`.

Under the hood, these methods transform the ASCII value of the character(s) from lowercase to uppercase. Make sure your string is encoded in a compatible format to avoid any unexpected behavior.

## See Also
More on Ruby Strings: [Official Ruby Documentation](https://ruby-doc.org/core/String.html). 
For `titleize` and other ActiveSupport goodness: [Ruby on Rails API Documentation](https://api.rubyonrails.org/classes/ActiveSupport/Inflector.html#method-i-titleize). 
More on ASCII values: [ASCII Table and Description](http://www.asciitable.com/).