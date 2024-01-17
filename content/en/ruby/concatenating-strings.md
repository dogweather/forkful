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

## What & Why?

Concatenating strings is the process of joining multiple strings together to create a single string. Programmers commonly use this technique when they need to combine multiple pieces of text or data into a single variable.

## How to:

Concatenating strings in Ruby is quite simple. You can use the `+` operator to combine two or more strings. Let's take a look at an example:

```Ruby
first_name = "John"
last_name = "Doe"

full_name = first_name + last_name
puts full_name
```
This code will output `JohnDoe` because the `+` operator simply joins the two strings together without any space in between. To add a space, we can do the following:

```Ruby
first_name = "John"
last_name = "Doe"

full_name = first_name + " " + last_name
puts full_name
```
Now, the output will be `John Doe` with a space between the two names. We can also use the `<<` operator as a shortcut for concatenation:

```Ruby
first_name = "John"
last_name = "Doe"

full_name = first_name << " " << last_name
puts full_name
```
The output will be the same as the previous example. It is important to note that concatenating strings does not change the original variables, it only creates a new string.

## Deep Dive:

Concatenating strings has been a common practice in programming for a long time. In the early days, concatenation was done using functions such as `CONCAT` in SQL or `concat()` in C. However, with the introduction of the `+` operator, it has become much simpler and cleaner.

Alternative ways to concatenate strings in Ruby include using the `concat` method or the `sprintf` method. The `concat` method is similar to the `<<` operator and the `sprintf` method uses a format string to combine strings.

Ruby also has the `join` method, which allows you to concatenate multiple strings with a specified separator. This can be useful when you have an array of strings that you want to join together.

In terms of implementation, when you concatenate two strings, a new string object is created in memory. If you have a large number of strings to concatenate, it is more efficient to use the `<<` operator or the `concat` method instead of using the `+` operator, as it creates a new string object every time it is used.

## See Also:

To learn more about concatenating strings in Ruby, check out the [official documentation](https://ruby-doc.org/core-2.7.2/String.html#method-i-2B) and the [Ruby Style Guide](https://rubystyle.guide/#string-concatenation). You can also explore other string manipulation methods in Ruby, such as interpolation and formatting. Happy coding!