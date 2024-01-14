---
title:    "Ruby recipe: Extracting substrings"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why

As a Ruby programmer, understanding how to manipulate strings is an essential skill. One common task is extracting substrings, or a portion of a larger string. Whether you need to extract a specific word, character, or set of characters, knowing how to do so will save you time and make your code more efficient.

## How To

To extract substrings in Ruby, we can use the `slice` or `[]` methods. These methods function similarly and allow us to specify the starting and ending index of the substring we want to extract. Let's take a look at an example:

```ruby
sentence = "I love programming"
puts sentence.slice(2, 4) # Output: love
puts sentence[6..12] # Output: programming
```

In the first line, we use the `slice` method to extract the word "love" starting at index 2 and including the next 4 characters. Similarly, the second line uses the `[]` method with a range to extract the word "programming" starting at index 6 and including the next 7 characters.

We can also use negative numbers to specify the starting index from the end of the string. For example, `sentence[-3..-1]` will extract the last 3 characters of the string, "ing".

Another useful method for substring extraction is the `split` method. This method splits a string into an array based on a delimiter. We can then access specific elements of the array, which would be substrings of the original string.

```ruby
sentence = "Programming is fun"
words = sentence.split(" ") # Output: ["Programming", "is", "fun"]
puts words[1] # Output: is
```

## Deep Dive

Behind the scenes, all strings in Ruby are objects of the `String` class, which has many built-in methods for manipulating strings. The `slice` and `[]` methods are just two examples of how we can extract substrings. We can also use the `scan` method to extract multiple occurrences of a substring in a string.

```ruby
sentence = "Ruby is the best language for writing web applications"
matches = sentence.scan("b") # Output: ["b", "b"]
```

In this example, `scan` looks for any occurrence of the letter "b" in the sentence and returns them as an array. This method is useful when we need to extract multiple substrings and store them in a data structure.

It's also important to note that substrings in Ruby are immutable, meaning we cannot modify them directly. If we want to change a substring, we would need to first extract it, modify it, and then reassign it to the original string.

## See Also

For more information on working with strings in Ruby, check out the following resources:

- [Ruby String Documentation](https://ruby-doc.org/core-2.7.1/String.html)
- [Ruby String Methods](https://www.rubyguides.com/ruby-string-methods/)
- [Ruby String Cheat Sheet](https://www.shortcutfoo.com/app/dojos/ruby-strings/cheatsheet)