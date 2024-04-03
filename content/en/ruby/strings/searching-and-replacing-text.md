---
date: 2024-01-20 17:58:26.553026-07:00
description: "Searching and replacing text is swapping specific words or phrases with\
  \ alternatives in a string. Programmers do it to update data, fix errors, or change\u2026"
lastmod: '2024-03-13T22:45:00.535306-06:00'
model: gpt-4-1106-preview
summary: Searching and replacing text is swapping specific words or phrases with alternatives
  in a string.
title: Searching and replacing text
weight: 10
---

## How to:
Ruby makes it easy. Use `gsub` to globally substitute text, or `sub` for a single instance. Here's a quick look:

```ruby
# Original string
phrase = "Hello, world!"

# Replace 'world' with 'Ruby'
puts phrase.gsub('world', 'Ruby')
# => Hello, Ruby!

# Replace only first occurrence of 'l'
puts phrase.sub('l', '7')
# => He7lo, world!
```
The output? The first print shows `"Hello, Ruby!"`, the second gives `"He7lo, world!"`.

## Deep Dive
The `gsub` and `sub` methods have been with Ruby since its early days, mirroring the substitution concept from older languages like Perl. Alternatives? Sure, you could use a regex for more complex patterns, or even patch together `split` and `join` if you're feeling crafty.

What's cool is Ruby's block capability with `gsub`. Instead of a simple find-and-replace, you can do some heavy lifting inside that block:

```ruby
# Capitalize each word
puts "make me pretty".gsub(/\b\w/) { |match| match.upcase }
# => Make Me Pretty
```

Why bother? For starters, using regex with `gsub` lets you tackle nuanced cases where you need more finesse than blunt 'find this, replace with that'.

## See Also
Sharpen those skills - dive into the docs or check out these resources:
- [Ruby String#gsub documentation](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Regular Expressions in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)

Got it? Good. Now go play with some strings.
