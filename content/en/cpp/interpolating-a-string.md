---
title:                "Interpolating a string"
html_title:           "C++ recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpolating a string is the process of combining multiple strings or values into one cohesive string. This allows programmers to create dynamic and personalized output based on user input or other variables. It saves time and effort by reducing the need for writing out long, repetitive strings.

## How to:
```C++
#include <iostream>
using namespace std;

int main() {
  string name = "John";
  int age = 25;
  
  // Using concatenation
  cout << "Hello, my name is " + name + " and I am " + to_string(age) + " years old.";

  // Using interpolation
  cout << "Hello, my name is " << name << " and I am " << age << " years old.";
  
  return 0;
}

// Output:
// Hello, my name is John and I am 25 years old.
```

## Deep Dive:
Interpolating strings has been around since the early days of programming. It became more popular with the introduction of string formatting languages like Perl and Python. In C++, there are other alternatives such as the `sprintf` function, but interpolation offers a simpler and more intuitive approach.

It is important to note that interpolation can improve the readability of code, but it can also lead to security vulnerabilities if not used properly. This is because interpolated strings are susceptible to injection attacks, so it is important to sanitize user input when using interpolation.

## See Also:
- [C++ Reference- String Interpolation](https://en.cppreference.com/w/cpp/language/string_literal)
- [A Beginner's Guide to String Interpolation in C++](https://www.bogotobogo.com/cplusplus/StringInterpolation.php)
- [Interpolation (computer programming)](https://en.wikipedia.org/wiki/Interpolation_(computer_programming))