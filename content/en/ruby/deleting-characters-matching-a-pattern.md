---
title:                "Ruby recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Sometimes in programming, we encounter situations where we need to modify or clean up our data. One common task is to delete characters that match a certain pattern. This can be useful in tasks such as data cleaning, text manipulation, or string parsing. In Ruby, there are several ways to accomplish this task, and in this blog post, we will explore the different methods for deleting characters matching a pattern.

## How To

First, let's clarify what we mean by "matching a pattern". In Ruby, we can use regular expressions (regex) to define a pattern of characters to match. So, when we say "characters matching a pattern", we are talking about a string of characters that follow a specific format.

To delete characters matching a pattern, we can use the `gsub` method. This method stands for "global substitution", meaning it will replace all instances of the pattern in a string. The syntax for this method is as follows:

```ruby
str.gsub(pattern, replacement)
```

Let's say we have a string that contains a mix of letters and numbers, and we want to remove all the numbers from it. We can do this using regex and the `gsub` method:

```ruby
str = "a1b2c3d4e5"
str.gsub(/\d+/, '')
```

In this example, we used the regex pattern `\d+` to match one or more digits, and we replaced them with an empty string, effectively deleting them from the original string. The output of this code will be:

```ruby
"abcde"
```

Another way to delete characters matching a pattern is by using the `tr` method. This method stands for "translate", and it replaces characters in a string based on a mapping of characters. The syntax for this method is as follows:

```ruby
str.tr(old_chars, new_chars)
```

Similar to the `gsub` method, we can use regex to define a pattern for the `tr` method. For example, to remove all numbers from a string, we can do the following:

```ruby
str = "a1b2c3d4e5"
str.tr("0-9", '')
```

In this code, we used the regex pattern `0-9` to match all digits, and we replaced them with an empty string. The output of this code will be the same as the previous example:

```ruby
"abcde"
```

## Deep Dive

Now that we have covered the basics of how to delete characters matching a pattern, let's take a deeper look at regex and how we can use it to define more complex patterns. As mentioned earlier, regex is a powerful tool for matching patterns in strings and is widely used in many programming languages.

Regex patterns are defined within a pair of forward slashes, and we can use different modifiers to modify our pattern. For example, the `+` modifier means "one or more", the `*` modifier means "zero or more", and the `?` modifier means "zero or one". We can also use square brackets to specify a range of characters. For example, `[a-z]` means all lowercase letters, and `[0-9]` means all digits.

Furthermore, we can use the `|` operator to specify alternatives. For example, `(a|b)` means either "a" or "b", and `(abc|def)` means either "abc" or "def". The `^` and `$` symbols represent the start and end of strings, respectively. So, `^a` means the string starts with "a", and `b$` means the string ends with "b".

There are many more regex modifiers and symbols that we can use to create complex patterns. If you want to learn more, there are plenty of resources available online.

## See Also

- [Ruby Documentation on Regular Expressions](https://ruby-doc.org/core-2.7.1/Regexp.html)
- [Regular Expressions Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/ruby)
- [Tutorial on Using Regular Expressions in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)