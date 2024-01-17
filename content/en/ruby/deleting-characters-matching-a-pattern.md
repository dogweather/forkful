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

## What & Why?
Deleting characters matching a pattern is the process of removing specific characters from a string or text based on a given pattern. Programmers use this to clean up data or manipulate text in a more efficient manner. It helps to streamline code and make it more readable.

## How to:
To delete characters matching a pattern in Ruby, we can use the `gsub` method. This method takes two arguments - the pattern and the replacement string. Here's an example of how we can use it:

```Ruby
string = "Hello, World!"
new_string = string.gsub("l", "")
puts new_string
```
Output:
```Ruby
Heo, Word!
```
In the above code, we used the letter "l" as the pattern and an empty string as the replacement. This replaces all occurrences of "l" in the string with an empty string, effectively deleting them.

We can also use regular expressions as patterns to delete characters matching a certain pattern. For instance, if we want to remove all digits from a string, we can use the regex `\d` as the pattern. Here's an example:

```Ruby
string = "Hello123World456"
new_string = string.gsub(/\d/, "")
puts new_string
```
Output:
```Ruby
HelloWorld
```

## Deep Dive:
The `gsub` method is part of the `String` class in Ruby and is also available in other programming languages such as JavaScript and PHP. It stands for "global substitution" and is similar to the `replace` method, but with the ability to use regular expressions.

An alternative to using `gsub` for deleting characters matching a pattern is to use the `delete` method. This method takes a string or a character as an argument and removes all occurrences of it from the original string. However, it does not support the use of regular expressions.

When using `gsub` with regular expressions, keep in mind that the pattern is case-sensitive. So if you want to delete both uppercase and lowercase letters, you can use the `i` flag to make the regex case-insensitive.

## See Also:
- [Ruby docs on `gsub` method](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
- [Regular expression tutorial](https://www.regular-expressions.info/tutorial.html)
- [Online regex tester](https://regex101.com/)