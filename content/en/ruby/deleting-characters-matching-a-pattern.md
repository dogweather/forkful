---
title:                "Deleting characters matching a pattern"
html_title:           "Ruby recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

There are many reasons why someone may need to delete characters that match a certain pattern in their code. It could be to remove special characters or unwanted symbols, to update and organize data, or to simply improve the readability or functionality of their code.

## How To

```Ruby
# To delete characters matching a specific pattern, we can use the gsub method.
# In this example, we will remove all vowels from a string and return the modified string.

original_string = "Hello World!"

modified_string = original_string.gsub(/[aeiou]/, "")

puts modified_string

# Output: "Hll Wrld!"

# We can also use the gsub! method to modify the original string without creating a new one.
# This is useful if we want to make permanent changes to our code.

original_string.gsub!(/[aeiou]/, "")

puts original_string

# Output: "Hll Wrld!"
```

## Deep Dive

The gsub method in Ruby uses regular expressions (regex) to search and replace characters in a string. The pattern we specify inside the square brackets ([]) indicates which characters to delete. In the above example, /[aeiou]/ means any vowels. We can also specify specific characters or ranges, such as /[a-z]/ which would delete all lowercase letters.

It's important to note that the gsub method is case sensitive, so /[aeiou]/ would only match lowercase vowels. To match both uppercase and lowercase vowels, we can use the i modifier like this: /[aeiou]/i.

## See Also

- [How to Use Regular Expressions in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Ruby String#gsub Method Documentation](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Regular Expressions Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)