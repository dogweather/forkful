---
date: 2024-02-03 19:02:29.358527-07:00
description: "Capitalizing a string usually means converting the\
  \ first character of a string to uppercase and the rest to lowercase. But sometimes\u2026"
lastmod: '2024-03-25'
model: gpt-4-0125-preview
summary: "Capitalizing a string usually means converting the first\
  \ character of a string to uppercase and the rest to lowercase. But sometimes\u2026"
title: Capitalizing a string
changelog:
- 2024-03-25, dogweather, edited and tested
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string often means converting the first character of a string to uppercase and the rest to lowercase. But sometimes it can mean just making sure that the first character is uppercase while leaving the rest of the string unchanged. Honestly, it's a vague term, in my opinion.

## How to:
Ruby provides [straightforward methods for string manipulation](https://docs.ruby-lang.org/en/3.3/String.html), including capitalization:

```ruby
# Ruby's built-in method
string = "hello world"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Ruby's `.capitalize` method is convenient but only affects the first letter. For more control or to capitalize each word in a string (known as title case), you might want to use the `titleize` method from the Rails ActiveSupport extension, or implement it yourself:

```ruby
# Using ActiveSupport's 'titleize' in Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

Here's what a home-made solution looks like:

```ruby
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

This method splits the string into an array of words, capitalizes each one, then joins them back together with a space.

Personally, I take this idea much farther in my code. I wrote [my own `titleize` method which accounts for small words like "a" and "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
