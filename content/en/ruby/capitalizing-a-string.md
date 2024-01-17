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

Capitalizing a string means converting the first character of the string to uppercase and leaving the rest of the characters unchanged. Programmers often do this to make strings more visually appealing or to follow certain formatting rules and conventions.

## How to:

To capitalize a string in Ruby, we can use the `capitalize` method. It takes the first character of the string and converts it to uppercase, leaving the rest unchanged. Let's see an example:

```Ruby
str = "hello world"
puts str.capitalize
```
Output: "Hello world"

We can also use the `capitalize!` method, which modifies the original string instead of creating a new one. This can be useful if we want to capitalize a string in-place without using additional memory. Let's try it out:

```Ruby
str = "hello world"
str.capitalize!
puts str
```
Output: "Hello world"

## Deep Dive

The `capitalize` method has been a part of Ruby since its first release in 1995. It was created by Yukihiro Matsumoto, also known as "Matz," who is the designer and lead developer of Ruby. This method is a simple and efficient way to capitalize strings in Ruby.

An alternative to the `capitalize` method is using the `upcase` method, which converts all characters in a string to uppercase. However, this method does not follow certain formatting rules, such as capitalizing the first character only. Additionally, the `capitalize` method is more specific and precise, making it a better choice in most cases.

Implementation-wise, the `capitalize` method uses the `unicode_normalize` method to handle special characters and variations of capitalization in different languages. This ensures that the method works correctly for all types of strings.

## See Also

To learn more about string manipulation in Ruby, refer to the official documentation: https://ruby-doc.org/core-3.0.0/String.html

You can also check out some useful string methods, such as `split` and `gsub`, to enhance your string manipulation skills.

For a more in-depth understanding of the history of Ruby and its creator "Matz," you can read the book "The Book of Ruby" by Huw Collingbourne.