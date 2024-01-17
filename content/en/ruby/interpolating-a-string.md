---
title:                "Interpolating a string"
html_title:           "Ruby recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpolating a string in Ruby refers to inserting values or variables into a string to create dynamic content. This allows programmers to generate strings with changing information, making their code more efficient and flexible. It is a common practice in Ruby and is used in many different scenarios, such as displaying user input or generating error messages.

## How to:
To interpolate a string in Ruby, use the "#" and "{}" symbols to wrap the variable or value inside the string. For example:
```Ruby
name = "John"
puts "Hello #{name}"
```
This will output: "Hello John"
You can also use interpolation with method calls or mathematical operations:
```Ruby
age = 25
puts "John is now #{age + 5} years old"
```
This will output: "John is now 30 years old"

## Deep Dive:
Interpolating strings has been a feature in Ruby since its early versions. It is heavily influenced by the Perl programming language, which also uses the "#" and "{}" symbols for string interpolation. However, unlike Perl, Ruby supports more complex expressions within the {} brackets, such as method calls and mathematical operations.

An alternative to string interpolation in Ruby is the use of the concatenation operator "+". While this achieves the same result, it is not as efficient and can be tedious in cases where multiple variables or values need to be inserted into a string. String interpolation simplifies and streamlines the process.

Behind the scenes, string interpolation uses the method `to_s` to convert the inserted value or variable into a string before it is added to the string. This is important to keep in mind when using interpolation with custom objects or classes as the `to_s` method can be overridden to return a specific string representation.

## See Also:
- [Ruby Documentation on String Interpolation](https://ruby-doc.org/core-<DateTime>/doc/syntax/literals_rdoc.html#label-String+Literals)
- [History of String Interpolation in Programming Languages](https://en.wikipedia.org/wiki/String_interpolation)
- [Alternative to String Interpolation using Concatenation](https://learnrubythehardway.org/book/ex4.html)