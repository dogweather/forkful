---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string means discovering the number of characters in a given string. Programmers often do this to determine the size of a data input or to loop over a string.

## How to:

Easy as pie. In Ruby, you just need to use the `length` or `size` method. Check it out: 

```Ruby
str = "Hello, World!"
puts str.length  #Output: 13
puts str.size    #Output: 13
```
Both `length` and `size` return the number of characters in a string.

## Deep Dive

Ruby, being a high-level, dynamically-typed language, has a couple of easy ways to find string length. 

Historically, `length` and `size` have been implemented as aliases - they do the same thing. Why two methods? A holdover from Ruby’s influences, Perl and Smalltalk, which used these terms respectively. 

Alternatives? There's `str.bytesize`, which gives the length in bytes instead of characters. Useful for strings with non-ASCII characters.

```Ruby
str = "こんにちは"
puts str.length    #Output: 5
puts str.bytesize  #Output: 15
```
Remember that string length is an O(1) operation in Ruby - fast and efficient because string length is stored as an attribute.

## See Also

You can explore more about Ruby strings in the official Ruby documentation [(Ruby-Doc)](https://ruby-doc.org/core-2.7.0/String.html) as well as in various Ruby tutorials and guidebooks. For a comparison between `size`, `length`, and `bytesize`, check out these articles on [StackOverflow](https://stackoverflow.com/questions/591607/whats-the-difference-between-size-length-and-count).