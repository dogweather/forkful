---
date: 2024-01-20 17:51:31.620513-07:00
description: "String interpolation lets you embed variables or expressions inside\
  \ a string. We do this for cleaner, more readable code that glues together dynamic\u2026"
lastmod: '2024-03-13T22:45:00.536135-06:00'
model: gpt-4-1106-preview
summary: "String interpolation lets you embed variables or expressions inside a string.\
  \ We do this for cleaner, more readable code that glues together dynamic\u2026"
title: Interpolating a string
weight: 8
---

## What & Why?

String interpolation lets you embed variables or expressions inside a string. We do this for cleaner, more readable code that glues together dynamic content with static text.

## How to:

In Ruby, you wrap your variable or expression in `#{}` and plunk it down where you want it in a double-quoted string. Like so:

```Ruby
name = "Jesse"
greeting = "Hey there, #{name}!"
puts greeting # => Hey there, Jesse!
```

You're not limited to just variables; any Ruby code can go in there:

```Ruby
price_per_kg = 5
quantity = 2
puts "Your total is: $#{price_per_kg * quantity}" # => Your total is: $10
```

Remember, single quotes won't work:

```Ruby
puts 'Hey there, #{name}!' # => Hey there, \#{name}!
```

## Deep Dive

Back in the day, we'd concatenate strings and variables using `+` or `<<`, making things messy fast.

```Ruby
email = "user" + "@" + "example.com"
```

Enter string interpolation in Ruby, a more refined way to merge text with code. Ruby evaluates whatever's inside `#{}` and converts it to a string automatically. Consider the work it saves from converting and concatenating strings:

```Ruby
"pi is roughly #{Math::PI.round(2)}"
```

Ruby's not unique; many languages have their own flavor of this handy feature. But caution: unlike some languages, Ruby strictly reserves this magic for double-quoted strings and certain other cases (like backticks and symbols). Single-quotes just spit out what's inside them, curly braces and all.

## See Also

- Ruby Documentation on syntax: [Ruby Docs - Syntax](https://ruby-doc.org/core-3.1.2/doc/syntax/literals_rdoc.html#label-Strings)
- A deeper look into string manipulation: [Ruby-Doc.org - String](https://ruby-doc.org/core-3.1.2/String.html)
