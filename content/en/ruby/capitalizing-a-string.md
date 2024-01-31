---
title:                "Capitalizing a string"
date:                  2024-01-19
simple_title:         "Capitalizing a string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string means making the first character uppercase and the rest lowercase. Programmers do it to format output for consistency or to meet certain data standards.

## How to:

In Ruby, you capitalize a string with the `.capitalize` method:

```Ruby
puts "hello world".capitalize  # Output: "Hello world"
```

To capitalize all words in a string, use:

```Ruby
puts "hello world".split.map(&:capitalize).join(' ')  # Output: "Hello World"
```

Note that `.capitalize` only affects the first word:

```Ruby
puts "hello WORLD".capitalize  # Output: "Hello world"
```

## Deep Dive

Capitalizing strings has been necessary since computers started interacting with humans. It ensures proper nouns and sentences start correctly, meeting grammar standards.

In some languages, like Ruby, `.capitalize` is built-in. Others need custom functions or libraries. The method in Ruby also downcases the rest of the string which can be seen in the above examples.

An alternative in Ruby is to use the `titleize` method from the `ActiveSupport::Inflector` methods, mostly used in Rails:

```Ruby
require 'active_support/core_ext/string/inflector'
puts "hello world".titleize  # Output: "Hello World"
```

However, `titleize` is more heavyweight and not part of Ruby's standard library.

Implementation-wise, when you call `.capitalize`, Ruby creates a new string with the first character converted to uppercase and the remainder to lowercase. It's handy for ensuring formatting remains consistent in user interfaces and data processing.

## See Also

- Ruby's documentation on `.capitalize`: [Ruby Docs - capitalize](https://ruby-doc.org/core-2.7.0/String.html#method-i-capitalize)
- About `ActiveSupport::Inflector` and `titleize`: [API Dock - titleize](https://apidock.com/rails/String/titleize)
- To learn about Ruby's other string methods: [Ruby Docs - String](https://ruby-doc.org/core-2.7.0/String.html)
