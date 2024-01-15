---
title:                "Concatenating strings"
html_title:           "Ruby recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings is a common task in programming, and it is essential for building dynamic and flexible applications. By combining different pieces of text, we can create longer and meaningful strings that provide useful information to users.

## How To

To concatenate strings in Ruby, we can use the plus (+) operator or the concat method. Let's take a look at some code examples below:

```Ruby
# Using the plus operator
str1 = "Hello"
str2 = "World"
result = str1 + " " + str2
puts result
# Output: Hello World

# Using the concat method
str1 = "Hello"
str2 = "World"
str1.concat(" ", str2)
puts str1
# Output: Hello World
```

In these examples, we are combining two strings "Hello" and "World" to create a new string "Hello World." We can also use string interpolation to concatenate strings, as shown below:

```Ruby
str1 = "Hello"
str2 = "World"
result = "#{str1} #{str2}"
puts result
# Output: Hello World
```

Here, the variables str1 and str2 are inserted into the string using the #{} notation. This method is useful when we want to include variables or expressions in our concatenated string.

Another way to concatenate strings is by using the shovel (<<) operator. This method modifies the original string and appends the second string to it, as shown below:

```Ruby
str1 = "Hello"
str2 = "World"
str1 << " " << str2
puts str1
# Output: Hello World
```

## Deep Dive

In Ruby, strings are mutable, which means they can be modified after they are created. This allows us to concatenate strings without creating a new string object every time.

One important thing to note while concatenating strings is that we need to be aware of the data types we are combining. Ruby converts non-string objects to strings before concatenating, which can lead to unexpected results.

For example, when we try to concatenate a number with a string, the number is converted to a string and then appended to the original string.

```Ruby
str = "Hello"
num = 123
str.concat(" ", num)
puts str
# Output: Hello 123 
```

Thus, it is essential to ensure that the data types are compatible before concatenating strings.

## See Also

- [Ruby String Concatenation](https://www.rubyguides.com/2019/02/ruby-string-concatenation/)
- [Ruby String Interpolation](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-strings/cheatsheet)
- [Ruby Strings](https://ruby-doc.org/core-3.0.0/String.html)