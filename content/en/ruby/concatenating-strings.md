---
title:                "Concatenating strings"
date:                  2024-01-20T17:35:38.349648-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenating strings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Concatenating strings is just a fancy way of saying 'sticking them together end-to-end'. Programmers do it to combine words and sentences, to build messages, or to dynamically insert values into text.

## How to:
In Ruby, you can concatenate strings with the `+` operator or the `<<` method, which modifies the string in place. Here's how to connect the dots—or rather, the words:

```Ruby
# Using the + operator, which returns a new string
greeting = "Hello, " + "world!"
puts greeting # Output: Hello, world!

# Using the << method, which alters the original string
name = "Alice"
name << ", meet Bob"
puts name # Output: Alice, meet Bob
```

## Deep Dive
Concatenation has been in Ruby since its birth. But with time, the language has provided more ways to weave strings together.

We've covered `+` and `<<`, but there's also `concat` method and interpolation.

- Using `concat`: This method is like `<<` but allows you to tack on multiple strings at once.
```Ruby
phrase = "Roses are red"
phrase.concat(", violets are blue")
puts phrase # Output: Roses are red, violets are blue
```

- Interpolation: Puts variables into a string without directly concatenating them. It's neater and preferred for inserting variables:
```Ruby
mood = "excited"
message = "I am #{mood} to learn Ruby!"
puts message # Output: I am excited to learn Ruby!
```

Interpolation automatically calls `to_s` on any variable, ensuring non-string types play nice inside a string.

Also, remember—it's not just about sticking words together; Ruby keeps an eye on performance too. When you use `+`, Ruby creates a new string. Over time or in loops, this can be memory-hungry. In contrast, `<<` and `concat` modify the original string, which is often more efficient.

## See Also
- The Ruby documentation on String: https://ruby-doc.org/core-3.1.2/String.html
- An article on Ruby string interpolation: https://www.rubyguides.com/2018/11/ruby-string-interpolation/
- A guide to Ruby operators: https://www.tutorialspoint.com/ruby/ruby_operators.htm
