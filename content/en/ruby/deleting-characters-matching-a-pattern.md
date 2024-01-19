---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern, often referred to as "pattern matching", is the task of removing specific set(s) of characters from a string based on some pattern. Programmers use it for data cleansing, parsing, or to simplify text data.

## How to:

In Ruby, we can use the `String#tr` or `String#gsub` methods. Here's a simple demo where we remove all vowels from a text.

```Ruby
text = "Hello, world!"
no_vowels = text.gsub(/[aeiou]/i, '') # using regex, case insensitive
puts no_vowels  # Outputs: "Hll, wrld!"
```

And here's how to do that with `String#tr`:

```Ruby
no_vowels = text.tr('aeiouAEIOU', '')  # case sensitive
puts no_vowels  # Outputs: "Hll, wrld!"
```

## Deep Dive

Pattern matching had existed long before Ruby. Perl, for instance, has offered robust regex capabilities that inspired many other languages, including Ruby.

Alternatives to `gsub` and `tr` include: `delete` (for exact matches), `squeeze` (for consecutive duplicates), and `sub` (for first matches). 

A peculiarity about `gsub` and `tr` is that they return fresh strings. They don't mutate the original unlike their bang versions i.e., `gsub!` and `tr!`. 

## See Also

1. Ruby Docs - `String#gsub`: https://ruby-doc.org/core-2.6.3/String.html#method-i-gsub
2. Ruby Docs - `String#tr`: https://ruby-doc.org/core-2.2.0/String.html#method-i-tr
3. Ruby Guides - Regex: https://www.rubyguides.com/2015/06/ruby-regex/ 
4. Ruby Docs - `String#delete`: https://ruby-doc.org/core-2.6.3/String.html#method-i-delete 

Note: Always refer to the installation's local docs by typing `ri String#gsub` or `ri String#tr` in the terminal.