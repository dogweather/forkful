---
title:                "Capitalizing a string"
aliases:
- /en/ruby/capitalizing-a-string.md
date:                  2024-02-03T19:02:29.358527-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizing a string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string in programming often refers to converting the first character of a string to uppercase and the rest to lowercase. Programmers do this for reasons like adhering to naming conventions, making outputs more readable, or ensuring data consistency for comparisons and storage.

## How to:
Ruby provides straightforward methods for string manipulation, including capitalization. Here’s how you can capitalize a string in Ruby:

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

If you're not using Rails or prefer a pure Ruby solution, here’s how you might capitalize each word in a string:

```ruby
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

This method splits the string into an array of words, capitalizes each one, then joins them back together with a space.
