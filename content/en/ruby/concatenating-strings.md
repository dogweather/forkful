---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenating Strings in Ruby

## What & Why?

Concatenating strings involves joining two or more strings together to form a single string. Programmers do it mainly to build dynamic strings, handle user inputs or output formatted messages.

## How to:

Let's look at two popular methods to concatenate strings in Ruby: the "+" operator and the "<<" operator, also known as the shovel operator.

```Ruby
# Using the "+" operator
first_name = "John"
last_name = "Doe"
full_name = first_name + " " + last_name
puts full_name
# Output: John Doe
```
Here, the "+" operator is used to concatenate the strings "John", " " and "Doe" together.

```Ruby
# Using the "<<" operator
first_name = "John"
last_name = "Doe"
full_name = first_name << " " << last_name
puts full_name
# Output: John Doe
```
The "<<" operator also concatenates the strings, but it modifies the original string.

## Deep Dive

Historically, in many programming languages, "+" was used to concatenate strings. However, in Ruby, the "<<" operator was introduced for efficient memory usage.

When you use "+", Ruby needs to create a new object. But with "<<" or "concat", the current string instance gets modified, saving memory space. If you're handling large strings or in a situation where performance matters, use "<<" instead of "+".

Another option is interpolation, especially useful when combining non-string objects, or for readability.
```Ruby
name = "John"
message = "Hello, #{name}!"
# Output: "Hello, John!"
```
Under the hood, Ruby implicitly calls the `"to_s"` method on the interpolated content inside the curly braces, converting it into a string, if it isn't already.

## See Also

For more detailed insights on string concatenation in Ruby, consider checking out these resources:

- Ruby's official documentation: https://docs.ruby-lang.org/en/
- An insightful StackOverflow thread on string concatenation: https://stackoverflow.com/questions/4684446/why-is-the-shovel-operator-preferred-over-plus-for-concatenating-ruby
- An article on Ruby string concatenation best practices: https://www.rubyguides.com/2018/06/rubys-string-concatenation/