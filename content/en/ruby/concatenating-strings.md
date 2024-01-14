---
title:    "Ruby recipe: Concatenating strings"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
Concatenating strings is a fundamental skill in any programming language, and Ruby is no exception. It allows you to combine multiple strings into one, making it easier to manipulate and display data in your programs.

## How To
To concatenate strings in Ruby, you can use the `+` operator or the `concat()` method. Let's look at some examples using both methods:

```
# Concatenating using the + operator
greeting = "Hello"
name = "John"
combined = greeting + name

puts combined # Output: HelloJohn

# Concatenating using the concat() method
greeting = "Hello"
name = "John"
greeting.concat(name)

puts greeting # Output: HelloJohn
```

In the first example, we use the `+` operator to combine the two strings, while in the second example we use the `concat()` method on the first string to add the second string to it. Both methods achieve the same result.

You can also use string interpolation to concatenate strings in Ruby:

```
greeting = "Hello"
name = "John"
combined = "#{greeting} #{name}"

puts combined # Output: Hello John
```

Here, we use the `#{}` syntax to insert the variables `greeting` and `name` into the string.

## Deep Dive
Under the hood, concatenation in Ruby is achieved by modifying the original string in memory rather than creating a new one. This means that it is more memory efficient than creating new strings every time concatenation is performed.

It is also important to note that concatenation only works with strings. If you want to combine strings with numbers, you will need to convert the numbers to strings first. This can be done using the `to_s` method or string interpolation.

```
number = 10
string = "My favorite number is "
combined = string + number.to_s

puts combined # Output: My favorite number is 10
```

## See Also
- [Ruby String Documentation](https://ruby-doc.org/core-2.7.0/String.html)
- [Ruby String Interpolation](https://ruby-doc.org/core-2.7.0/doc/syntax/literals_rdoc.html#label-Percent+Strings)