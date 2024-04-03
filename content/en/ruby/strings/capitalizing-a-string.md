---
changelog:
- 2024-03-25, dogweather, edited and tested
date: 2024-02-03 19:02:29.358527-07:00
description: 'How to: Ruby provides [straightforward methods for string manipulation](https://docs.ruby-lang.org/en/3.3/String.html),
  including capitalization.'
lastmod: '2024-03-25'
model: gpt-4-0125-preview
summary: Ruby provides [straightforward methods for string manipulation](https://docs.ruby-lang.org/en/3.3/String.html),
  including capitalization.
title: Capitalizing a string
weight: 2
---

## How to:
Ruby provides [straightforward methods for string manipulation](https://docs.ruby-lang.org/en/3.3/String.html), including capitalization:

```ruby
# Ruby's built-in method
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Very handy.

Ruby's `.capitalize` method is convenient but only upper-cases the first letter. For more control or to capitalize each word in a string (known as title case), you might want to use the `titleize` method from the Rails ActiveSupport extension, or implement it yourself:

```ruby
# Using ActiveSupport's 'titleize' in Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# A home-made solution
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

This method splits the string into an array of words, capitalizes each one, then joins them back together with a space.

Personally, I take this idea much farther in my code. I wrote my own [`titleize` method which accounts for small words like "a" and "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
