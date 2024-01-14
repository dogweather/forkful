---
title:                "Ruby recipe: Capitalizing a string"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

If you've ever worked with strings in Ruby, you may have come across the need to capitalize them. Capitalizing a string means converting the first letter of each word to uppercase, and it can be useful in various scenarios such as formatting names or creating titles. In this blog post, we'll delve into how you can easily capitalize a string in Ruby.

## How To

To capitalize a string in Ruby, we can make use of the `capitalize` method. This method takes no arguments and returns a copy of the string with the first letter capitalized. Let's see it in action:

```Ruby
string = "hello, world"
puts string.capitalize
```

The above code will output "Hello, world". As you can see, the first letter of the string has been converted to uppercase. However, it's important to note that the `capitalize` method only capitalizes the first letter of the string and leaves the rest of the letters unchanged.

If you want to capitalize all words in a string, you can use the `titleize` method from the ActiveSupport gem. This method takes care of capitalizing all words in a string and even handles special cases like acronyms. Here's an example:

```Ruby
require 'active_support/core_ext/string'
string = "ruby on rails"
puts string.titleize
```

The above code will output "Ruby on Rails". Notice how "on" is also capitalized, and "Rails" is not all uppercase.

## Deep Dive

The `capitalize` method uses the rules of the unicode version of the Simple Default Case Algorithm to capitalize the first character of the string. It ensures that characters with accents or special characters are also capitalized correctly.

When it comes to the `titleize` method, it uses a more complex set of rules to capitalize strings. It takes into account special cases like acronyms and ignores certain words like "a", "an", and "the" unless they are the first or last word in the string. You can find the full list of rules in the official documentation for the ActiveSupport gem.

## See Also

For more information on capitalizing strings in Ruby, check out the following resources:

- [Ruby's String documentation](https://ruby-doc.org/core-3.0.0/String.html)
- [Rails' ActiveSupport documentation](https://api.rubyonrails.org/v6.1.3.2/classes/String.html#method-i-titleize)
- [Unicode Standard Case Mappings](https://unicode.org/reports/tr21/)