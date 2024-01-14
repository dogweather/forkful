---
title:    "Ruby recipe: Deleting characters matching a pattern"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why
In programming, it is common to come across a task that involves manipulating strings. One such task is deleting characters that match a certain pattern. This can be useful in scenarios like data cleaning or validation. In this blog post, we will explore how to delete characters matching a pattern in Ruby programming.

## How To
The `gsub` method in Ruby can be used to delete characters matching a pattern in a string. Here's an example of how it works:

```Ruby
# Input string
str = "Hello,123 World!"

# Deleting all digits using gsub
new_str = str.gsub(/\d/, '')

# Output
puts new_str # Output: Hello, World!
```

In the above code, we use the `gsub` method along with a regular expression to find and delete all digits in the input string. The `//` signifies a regular expression, and `\d` represents any digit. We replace it with an empty string, effectively deleting it from the original string.

You can also use `gsub` to delete specific characters instead of using a pattern. Here's an example:

```Ruby
# Input string
str = "Hi@@@ there!!!"

# Deleting all '@' characters using gsub
new_str = str.gsub('@', '')

# Output
puts new_str # Output: Hi there!!!
```

In this code, we use `gsub` to find and delete all occurrences of the '@' character in the string.

There are also other useful methods, such as `sub` and `chop`, which can be used to delete characters from strings. The key is to understand what your specific task requires and choose the most appropriate method.

## Deep Dive
When using `gsub` to delete characters, it's essential to understand the regular expression used. Regular expressions are patterns that are used to find and manipulate strings. In our examples, we used `\d` to represent all digits and `@` to represent a specific character. You can use different combinations of characters and special symbols to create a regular expression that suits your needs.

You can also combine regular expressions to create more complex patterns. For instance, the following code deletes all vowels and digits in a string:

```Ruby
# Input string
str = "Hello123 World"

# Deleting all vowels and digits using gsub
new_str = str.gsub(/[aeiou\d]/, '')

# Output
puts new_str # Output: Hllo Wrld
```

In this regular expression, we use the `[]` symbol to specify that we want to find either a vowel (represented by `aeiou`) or a digit (represented by `\d`). This allows us to delete all occurrences of vowels and digits in a single line of code.

## See Also

- [String#gsub documentation](https://ruby-doc.org/core-2.7.2/String.html#method-i-gsub)
- [Ruby regular expressions tutorial](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Ruby string methods](https://ruby-doc.org/core-2.5.3/String.html)