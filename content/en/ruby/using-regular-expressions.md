---
title:                "Using regular expressions"
date:                  2024-01-19
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) are patterns used to match character combinations in strings. Programmers use them for searching, editing, or validating text because they're precise and efficient.

## How to:
Let's run through some Ruby regex basics.

```Ruby
# Finding a match
phrase = "Hello, World!"
puts phrase.match(/World/) # Output: World

# Replacement
puts phrase.gsub(/World/, "Ruby") # Output: Hello, Ruby!

# Extracting matches
email = "contact@example.com"
puts email.match(/\A[^@\s]+@([^@\s]+\.)+[^@\s]+\z/).to_s # Output: contact@example.com

# Iterating over matches
"Frodo, Gandalf, Arwen".scan(/\w+/) { |name| puts name }
# Output:
# Frodo
# Gandalf
# Arwen
```

## Deep Dive
Regular expressions in Ruby have been influenced by Perl's strong regex capabilities. Alternatives to regex include string methods like `#include?`, `#start_with?`, and `#end_with?`, but none offer the same power and flexibility. Ruby implements regex using its own library which is derived from Perl's regex engine, providing features like look-ahead and look-behind, non-greedy matching, and character class shortcuts.

## See Also
- [Ruby Regular Expressions](https://ruby-doc.org/core-3.1.0/Regexp.html): Official Ruby documentation for regex.
- [Rubular](http://rubular.com/): A Ruby-based regular expression editor, good for testing patterns.
