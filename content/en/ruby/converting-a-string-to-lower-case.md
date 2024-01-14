---
title:    "Ruby recipe: Converting a string to lower case"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why

Converting a string to lower case is a common task in programming, especially in text processing. This allows for consistency in comparing and manipulating strings, as well as presenting them in a standardized format.

## How To

```Ruby
# Using the built-in method .downcase
my_string = "Hello World"
puts my_string.downcase #=> "hello world"

# Using regular expression
my_string = "Hello World"
puts my_string.gsub(/[A-Z]/) { |letter| letter.downcase } #=> "hello world"
```

## Deep Dive

The built-in method `.downcase` is the simplest and most straightforward way to convert a string to lower case in Ruby. It takes into account language-specific characters and symbols, making it a reliable method for internationalization.

However, for more complex scenarios, using regular expressions may be necessary. Regular expressions allow for more fine-grained control over the conversion process. In the code example above, we use `gsub` to replace any uppercase letter with its lowercase counterpart using a block.

Some things to keep in mind when converting strings to lower case is that it is a non-destructive operation, meaning it creates a new string without altering the original one. Also, it only converts alphabetical characters and leaves digits and special characters unchanged.

## See Also

- [Ruby Documentation on `.downcase`](https://ruby-doc.org/core-3.0.1/String.html#method-i-downcase)
- [Ruby Documentation on `gsub`](https://ruby-doc.org/core-3.0.1/String.html#method-i-gsub)
- [Regular Expressions in Ruby](https://www.geeksforgeeks.org/ruby-regular-expressions/)
- [Internationalization in Ruby](https://www.rubyguides.com/2018/08/ruby-i18n/)