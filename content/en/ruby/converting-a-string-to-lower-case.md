---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case is changing all the uppercase characters within a string to lowercases. Programmers do this for a variety of reasons, like standardizing input, ensuring data consistency, or case-insensitive comparisons.

## How to:

Ruby has a method (`.downcase`) for this. Here's a basic example:

```Ruby
text = "Hello, World!"
lowercase_text = text.downcase
puts lowercase_text
```

When you run the above code, it will produce:

```
hello, world!
``` 

## Deep Dive

Ruby's downcase method has been around since its early versions. It's part of the standard String class that provides tools for manipulating text data. 

In the unicode world, `.downcase` may not behave as you expect. It converts characters based on the Unicode case mapping, which can have unexpected results for characters outside the Latin alphabet.

An alternative in Ruby is to use `.mb_chars.downcase.to_s`. This uses the multibyte safe method to convert strings to lowercase:

```Ruby
text = "ĞHello, World!"
lowercase_text = text.mb_chars.downcase.to_s
puts lowercase_text
```
On running this code, we get:

```
ğhello, world!
``` 

Another alternative is to use `.downcase(:fold)`. This performs Unicode case folding, which is a kind of normalization:

```Ruby
text = "ĞHello, World!"
lowercase_text = text.downcase(:fold)
puts lowercase_text
```

For `:fold`, the output is like `ğhello, world!`.

## See Also

For more information, see:

- [Ruby String downcase method (ruby-doc.org)](https://ruby-doc.org/core-2.7.0/String.html#method-i-downcase)
- [Working with unicode strings (rubyguides.com)](https://www.rubyguides.com/2015/05/working-with-unicode-strings/)