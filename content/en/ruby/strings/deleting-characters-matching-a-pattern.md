---
date: 2024-01-20 17:42:52.715524-07:00
description: "Deleting characters matching a pattern in strings is about surgically\
  \ removing bits you don't need, like stripping hashtags from tweets. Programmers\
  \ do it\u2026"
lastmod: '2024-03-13T22:45:00.534456-06:00'
model: gpt-4-1106-preview
summary: "Deleting characters matching a pattern in strings is about surgically removing\
  \ bits you don't need, like stripping hashtags from tweets. Programmers do it\u2026"
title: Deleting characters matching a pattern
weight: 5
---

## What & Why?
Deleting characters matching a pattern in strings is about surgically removing bits you don't need, like stripping hashtags from tweets. Programmers do it to clean data, format it consistently, or prepare it for further processing.

## How to:
```Ruby
# Simple deletion using String#gsub
example = "Hello, #World!"
cleaned_example = example.gsub(/#/, '') # => "Hello, World!"

puts cleaned_example # Output: Hello, World!

# Deleting a sequence of characters
sequence_example = "Th1s is 2 an example3."
cleaned_sequence = sequence_example.gsub(/[0-9]/, '') # => "This is an example."

puts cleaned_sequence # Output: This is an example.

# Deleting using String#delete
delete_example = "Remove vowels from this line."
cleaned_delete = delete_example.delete('aeiou') # => "Rmv vwls frm ths ln."

puts cleaned_delete # Output: Rmv vwls frm ths ln.
```

## Deep Dive
Historically, Ruby has been a language with a strong focus on text processing, inheriting some of its philosophies from Perl. That's why it gives you tools like `gsub` and `delete` right out of the box. 

`gsub` stands for global substitution. It's often used to substitute portions of strings that match a pattern (regular expression) with another string. When given an empty replacement string, it effectively deletes the matched characters.

`delete` is less flexible than `gsub` but faster when you just want to remove specific characters. You can't use regular expressions with `delete`, but for simple character removal, it's the straightforward choice.

There are other ways to skin this cat, though. Libraries like `scan` and `split` can dissect strings, and you can then reassemble them sans the unwanted characters. But for directly deleting characters, `gsub` and `delete` are your best mates.

## See Also
- Ruby's `gsub` documentation: [Ruby Doc gsub](https://ruby-doc.org/core-3.1.0/String.html#method-i-gsub)
- Ruby's `delete` documentation: [Ruby Doc delete](https://ruby-doc.org/core-3.1.0/String.html#method-i-delete)
- Regular Expressions in Ruby: [Ruby Regexp](https://ruby-doc.org/core-3.1.0/Regexp.html)
- "Programming Ruby: The Pragmatic Programmer’s Guide" for an in-depth look at Ruby's text processing capabilities.
