---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text is almost self-explanatory: you look for specific sequences in a text and exchange them for another. This is crucial in programming for data cleaning, regex matching, and refactoring. 

## How to:
Let's roll with Ruby to search and replace text in a string. It's a cakewalk with the `gsub()` method.

```Ruby
txt = "Love me, love my code."
puts txt.gsub("code", "dog")
```

Output:

```
Love me, love my dog.
```

For a more refined search and replace, we use regular expressions (regex). Let's cast off any non-alphanumeric characters.

```Ruby
txt = "C0d1ng++>>^R0ck$!"
puts txt.gsub(/[^0-9a-zA-Z]/, '')
```

Output:

```
C0d1ngR0ck
```

## Deep Dive:

The `gsub` method has been a part of Ruby's String class from the early days, a testament to the importance of search and replace. 

Alternatives? Sure. There's `sub()` which replaces only the first occurrence, unlike `gsub()` which replaces all occurrences. 

You can also pass a block to `gsub()`, causing it to replace each match with the result of invoking the block with the match. For example:

```Ruby
txt = "C++ Ruby Java Python"
puts txt.gsub(/\w+/) { |word| word.upcase }
```

Output:

```
C++ RUBY JAVA PYTHON
```

## See Also:

Ruby has a lot to offer with text manipulation. Dive deeper with these links:
- Ruby Documentation [`gsub`](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub): A detailed explanation of the method.
- StackOverflow [`ruby-regex`](https://stackoverflow.com/questions/tagged/ruby+regex): Need help? Use the tag `ruby-regex`.