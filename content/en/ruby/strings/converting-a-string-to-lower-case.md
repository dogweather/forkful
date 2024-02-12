---
title:                "Converting a string to lower case"
aliases:
- /en/ruby/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:01.725273-07:00
model:                 gpt-4-1106-preview
simple_title:         "Converting a string to lower case"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

In Ruby, converting a string to lower case means changing all the uppercase letters in the string to their lowercase counterparts. Programmers do this for consistency, especially in tasks like comparing user input or sorting.

## How to:

```ruby
# Using the downcase method
my_string = "Hello World!"
puts my_string.downcase  # => "hello world!"
```

```ruby
# Using downcase! for in-place transformation
my_string = "Hello World!"
my_string.downcase!
puts my_string           # => "hello world!"
```

## Deep Dive

Historically, case conversion has been a staple in programming languages to ensure text uniformity. It supports case-insensitive comparisons and searches, hence its importance.

The `downcase` and `downcase!` methods in Ruby stem from the language's principle of providing both non-destructive and destructive methods for string manipulation. The non-destructive `downcase` returns a new string, leaving the original untouched, while the destructive `downcase!` modifies the original string in place, which can be more memory efficient.

There are alternatives for cases when locale-specific rules apply. `String#mb_chars` combined with `ActiveSupport::Multibyte::Chars#downcase` from the Rails ActiveSupport library can handle more complex situations like characters with accents or other diacritical marks:
```ruby
require 'active_support/core_ext/string/multibyte'

my_string = "ÄÖÜ"
puts my_string.mb_chars.downcase  # => "äöü"
```

As for implementation, Ruby's `downcase` and `downcase!` internally use Unicode mapping to convert each character of the string to its lowercase equivalent.

## See Also

- Ruby documentation for `downcase` and `downcase!`: [Ruby Doc downcase](https://ruby-doc.org/core-3.1.2/String.html#method-i-downcase), [Ruby Doc downcase!](https://ruby-doc.org/core-3.1.2/String.html#method-i-downcase-21)
- For complex case conversions, see the ActiveSupport Core Extensions: [ActiveSupport String](https://api.rubyonrails.org/classes/String.html)
