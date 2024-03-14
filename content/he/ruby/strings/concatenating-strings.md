---
date: 2024-01-20 17:35:34.538305-07:00
description: "Concatenating strings means sticking them together end-to-end to make\
  \ a new string. It's like making a train by connecting cars. Programmers do this\
  \ to\u2026"
lastmod: '2024-03-13T22:44:40.186810-06:00'
model: gpt-4-1106-preview
summary: "Concatenating strings means sticking them together end-to-end to make a\
  \ new string. It's like making a train by connecting cars. Programmers do this to\u2026"
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
Concatenating strings means sticking them together end-to-end to make a new string. It's like making a train by connecting cars. Programmers do this to build texts, like creating messages or combining data into one string.

## How to: (איך לעשות:)
Ruby makes it easy. You can use `+`, `<<`, or `concat`. Here's how:

```Ruby
# Using +
greeting = "שלום " + "עולם!"
puts greeting # => שלום עולם!

# Using <<
first_name = "ישראל"
last_name = "ישראלי"
full_name = first_name << " " << last_name
puts full_name # => ישראל ישראלי

# Using concat
hello = "שלום"
world = " עולם"
hello.concat(world)
puts hello # => שלום עולם
```

## Deep Dive (צלילה עמוקה)
Originally, in older programming languages, strings were just arrays of characters. Concatenation was manual. But Ruby, with its user-friendly philosophy, made it much simpler.

Ruby's `+` is simple and clean, but creates a new string. The `<<` and `concat` methods modify the original string, which can be more efficient.

Don't forget about interpolation:

```Ruby
name = "מר ישראלי"
puts "ברוך הבא, #{name}" # => ברוך הבא, מר ישראלי
```

Interpolation is more Ruby-ish and often preferred for its readability and performance benefits.

## See Also (ראה גם)
- Ruby documentation on strings: [Ruby-Doc String](https://ruby-doc.org/core-3.1.0/String.html)
