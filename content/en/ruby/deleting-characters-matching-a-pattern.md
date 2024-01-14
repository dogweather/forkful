---
title:                "Ruby recipe: Deleting characters matching a pattern"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Have you ever come across a situation where you needed to remove certain characters from a string in your Ruby code? Maybe you were trying to clean up user input or extract only certain data from a larger string. Well, the good news is that Ruby has a built-in method that makes this task much simpler - `String#delete`.

## How To

To delete characters matching a pattern from a string, all you need to do is call the `delete` method on a string and pass in the pattern you want to delete as an argument. Let's see some examples:

````Ruby
# Delete the letter 'a' from a string
"Hello, my name is Amanda".delete("a")
# => "Hello, my nme is Amand"

# Delete all numbers from a string
"My phone number is 555-123-4567".delete("0-9")
# => "My phone number is -"

# Delete all vowels from a string
"I love Ruby programming!".delete("aeiou")
# => "l v Rby prgrmmng!"
````
As you can see, the `delete` method removes all occurrences of the characters or ranges you specify from the string. You can also pass in multiple arguments to delete multiple patterns at once.

## Deep Dive

Under the hood, the `delete` method uses the `tr` method to perform the deletion. `tr` stands for "translate" and it works by replacing any characters that match the first argument with the corresponding characters in the second argument. So in our examples above, the first argument is the pattern we want to delete and the second argument is an empty string since we don't want to replace it with anything.

It's worth noting that `delete` is not the same as `gsub` or `tr_s`. `gsub` will replace the deleted characters with an empty string while `tr_s` will condense multiple consecutive deleted characters into a single instance.

## See Also

- [Official Ruby documentation for String#delete](https://ruby-doc.org/core-2.5.1/String.html#method-i-delete)
- [Difference between delete and delete_suffix in Ruby](https://stackoverflow.com/questions/45666252/difference-between-delete-and-delete-suffix-in-ruby)
- [Ruby Monk tutorial on the `delete` method](https://rubymonk.com/learning/books/1-ruby-primer/chapters/8-strings/lessons/39-string-tricks)