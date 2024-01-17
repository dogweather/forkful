---
title:                "Interpolating a string"
html_title:           "Fish Shell recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpolating a string in Fish Shell refers to the process of inserting a variable or expression into a string, allowing the string to dynamically change based on the value of the inserted element. This is commonly done by programmers to easily and efficiently build complex strings without manually concatenating or manipulating each individual component.

## How to:
Coding examples:

```
# Basic Example
set greeting "Hello"
set name "John"
echo "$greeting $name" # Output: Hello John

# Expression Example
set x 2
set y 3
echo "$x + $y = $(expr $x + $y)" # Output: 2 + 3 = 5
```

## Deep Dive:
When it comes to string interpolation, Fish Shell's syntax is borrowed from other popular languages such as Ruby and Perl. This feature makes it easy for developers familiar with those languages to transition to Fish Shell. Other alternative methods for string interpolation include using printf formatting or string concatenation.

Internally, Fish Shell uses the variable expansion mechanism to perform string interpolation. This involves replacing the variable or expression with its corresponding value before the string is outputted.

## See Also:
- [Fish Shell documentation on variable expansion](https://fishshell.com/docs/current/docvars.html#string-interpolation)
- [Comparison of string interpolation methods](https://www.rubyguides.com/2019/02/ruby-string-interpolation/)