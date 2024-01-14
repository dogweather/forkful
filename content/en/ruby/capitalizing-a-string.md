---
title:    "Ruby recipe: Capitalizing a string"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why

Capitalizing a string may seem like a simple and trivial task, but it is an essential part of text formatting in many programming languages, including Ruby. It is a common practice to capitalize the first letter of a sentence, or all letters in an acronym or abbreviation. In this blog post, we will explore the various methods and techniques for capitalizing strings in Ruby.

## How To

In Ruby, there are multiple ways to capitalize a string, each with its own purpose and functionality. Let's take a look at some coding examples and their corresponding output.

```Ruby
# Using the capitalize method:

"hello world".capitalize
=> "Hello world"

# Using the upcase method:

"hello world".upcase
=> "HELLO WORLD"

# Using the capitalize! method (destructive):

str = "hello world"
str.capitalize!
=> "Hello world"
puts str
=> "Hello world"

# Using the upcase! method (destructive):

str = "hello world"
str.upcase!
=> "HELLO WORLD"
puts str
=> "HELLO WORLD"

# Capitalizing the first letter of each word in a sentence:

"hello world".split.map(&:capitalize).join(' ')
=> "Hello World"
```

As we can see, the capitalize method capitalizes only the first letter of a string, while the upcase method capitalizes all letters. The exclamation mark (!) in the destructive methods means that the original string is modified, rather than creating a new string. Lastly, we can use the split and join methods to capitalize the first letter of each word in a sentence.

## Deep Dive

Behind the scenes, the capitalize and upcase methods use the built-in Unicode algorithms for case mapping, which means they can handle special characters and languages other than English. In addition, Ruby also provides the capitalize word method, which capitalizes only the first letter of the string and downcases the rest of the letters.

```Ruby
# Using the capitalize_words method:

"HELLO WORLD".capitalize_words
=> "Hello world"
```

Another useful method is the titleize method, which capitalizes the first letter of each word in a string, downcases the rest of the letters, and handles special cases such as acronyms and conjunctions.

```Ruby
# Using the titleize method:

"ruby is an amazing programming language".titleize
=> "Ruby Is an Amazing Programming Language"
"the ruby language".titleize
=> "The Ruby Language"
"the ruby programming language".titleize
=> "The Ruby Programming Language"
"ruby on rails".titleize
=> "Ruby on Rails"
```

It is important to note that these methods do not only apply to plain strings, but also to strings that contain numbers, symbols, or even other languages.

## See Also

- [Ruby String Methods](https://ruby-doc.org/core-3.0.0/String.html)
- [Unicode Algorithms](http://www.unicode.org/standard/standard.html)
- [Titleize Method in Rails](https://guides.rubyonrails.org/active_support_core_extensions.html#titleize)