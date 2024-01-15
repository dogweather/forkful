---
title:                "Searching and replacing text"
html_title:           "Ruby recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Whether you're a seasoned developer or just starting to learn Ruby, being able to quickly manipulate and replace text within your code is a valuable skill. It can save you time and effort by avoiding manual changes and make your code more efficient. Plus, it's just plain cool.

## How To
To search and replace text in Ruby, you'll need to use the `gsub` method, which stands for "global substitution". Here's an example:

```Ruby
phrase = "I love Ruby!"
puts phrase.gsub("love", "adore")
```

This code will output "I adore Ruby!" by replacing the word "love" with "adore" in the `phrase` variable. You can also use regular expressions in `gsub` to perform more complex replacements, like removing punctuation or replacing specific patterns. For example:

```Ruby
phrase = "I have 5 dogs?"
puts phrase.gsub(/\d+/, "three")
```

This code will replace any number in the `phrase` variable with the word "three", resulting in the output "I have three dogs?".

## Deep Dive
The `gsub` method works by searching for a specific string or pattern and replacing it with a new value. It can also accept a block of code as an argument, allowing you to perform custom operations on the matching text. Here's an example:

```Ruby
phrase = "I love to code in Ruby!"
puts phrase.gsub(/\w+/) { |match| match.upcase}
```

This code will convert each word in the `phrase` variable to uppercase, resulting in the output "I LOVE TO CODE IN RUBY!".

You can also use `gsub` to make changes to specific parts of a string. For example, you can use regular expressions to target and replace a specific word, without touching similar words in the same string. This is especially useful when dealing with large text files.

## See Also
For more information about the `gsub` method and other string manipulation techniques in Ruby, check out the following resources:

- [Official Ruby Documentation for `gsub`](https://ruby-doc.org/core-3.0.0/String.html#method-i-gsub)
- [Ruby In-Depth: String Patterns and Regular Expressions](https://www.honeybadger.io/blog/ruby-string-patterns-regex/)
- [Ruby for Beginners: Manipulating Strings](https://www.rubyguides.com/2018/01/ruby-strings/)