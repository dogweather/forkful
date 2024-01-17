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

## What & Why?

Searching and replacing text is a common task for programmers, allowing them to quickly make changes to their code. This involves finding a specific string of text within a larger body of text and replacing it with a different string. Programmers do this to fix typos, refactor code, or make global changes throughout a project.

## How to:

To search and replace text in Ruby, you can use the `gsub` method. It takes in two arguments: the text to search for and the replacement text. Here's an example:

```
text = "Hello World! Welcome to Ruby!"
new_text = text.gsub("Hello", "Hi")
puts new_text

```

This code will print out: `Hi World! Welcome to Ruby!` as it replaces the word "Hello" with "Hi".

You can also use regular expressions with `gsub` to make more complex replacements. For example, if we wanted to replace all vowels with "*" in a string, we could do:

```
text = "Ruby is the best programming language!"
new_text = text.gsub(/[aeiou]/, "*")
puts new_text

```

This will output: `R*by *s th* b*st pr*gr*mm*ng l*ng**g*!`

## Deep Dive:

Searching and replacing text has been a crucial tool for programmers since the early days of programming. In Ruby, the `gsub` method is based on the Unix `sed` and `ed` commands. However, there are other methods and regular expression options available in Ruby for more specific or advanced replacements.

Alternatively, there are also programs and tools specifically designed for searching and replacing text, such as REPLika or Sublime Text. These can often have more features and a user-friendly interface for managing and organizing text replacements.

## See Also:

- [Ruby `gsub` method documentation](https://ruby-doc.org/core-3.0.0/String.html#method-i-gsub)
- [Regular expressions in Ruby](https://ruby-doc.org/core-3.0.0/Regexp.html)
- [Text editing software like REPLika](https://tuple.app/replika) or [Sublime Text](https://www.sublimetext.com/)