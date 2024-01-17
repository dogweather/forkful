---
title:                "Interpolating a string"
html_title:           "C recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpolating a string in C is the process of inserting variable values or expressions into a string at runtime. This can be useful for creating dynamic or customizable output. Programmers often use string interpolation to simplify the process of creating strings with variables, making their code more efficient and readable.

## How to:
In C, we can use the `printf()` function to interpolate strings by using the `%` modifier and specifying the corresponding variable or expression after the string. For example:

```C
int age = 25;
printf("I am %d years old.", age);
```

This would output: `I am 25 years old.`

We can also interpolate multiple variables or expressions within the same string by repeating the `%` modifier and corresponding values. For example:

```C
int height = 175;
char* unit = "cm";
printf("I am %d %s tall.", height, unit);
```

This would output: `I am 175 cm tall.`

## Deep Dive:
String interpolation has been a common feature in many programming languages, including C, since the early days of computing. It simplifies the process of creating strings with variables, making it easier for programmers to manipulate strings without having to concatenate different parts.

Alternatives to string interpolation in C include using the `sprintf()` function, which allows for the formatting of strings with variables or expressions, but requires the use of temporary buffers.

In terms of implementation, string interpolation in C works by using a combination of the `printf()` function and the `scanf()` function. While `printf()` allows for outputting strings with variables, `scanf()` allows for reading input and storing it in variables. These two functions work together to achieve string interpolation.

## See Also:
- [String Interpolation in C++](https://www.programiz.com/cpp-programming/string-interpolation)
- [String Formatting in C](https://www.geeksforgeeks.org/format-specifiers-in-c/)