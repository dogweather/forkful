---
title:                "Concatenating strings"
html_title:           "C++ recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Concatenating strings is the process of combining two or more strings into a single string. Programmers often do this to create a new string that contains the combined information from multiple strings, which can be useful for many reasons such as forming a sentence or creating a file name.

## How to:
To concatenate strings in C++, we can use the `+` operator or the `append()` function. Here's an example using the `+` operator:

```C++
// create two strings
string str1 = "Hello";
string str2 = "world";

// concatenate strings using the + operator
string result = str1 + str2;

// print the result
cout << result;
```

Output: `Helloworld`

We can also use the `append()` function to concatenate strings. Here's an example:

```C++
// create two strings
string str1 = "Hello";
string str2 = "world";

// concatenate strings using the append() function
str1.append(str2);

// print the result 
cout << str1;
```

Output: `Helloworld`

## Deep Dive:
Concatenating strings has been a fundamental concept in programming since the early days of computer programming. In the early programming languages, such as Fortran and COBOL, string concatenation was a more complex process that involved manipulating character arrays. However, with the introduction of languages like C++, concatenating strings became much simpler and more efficient.

While the `+` operator and `append()` function are the most common methods for concatenating strings in C++, there are alternative ways of achieving the same result. One alternative is using the `insert()` function to insert a string into another string at a specified position. Another alternative is using the `replace()` function to replace a portion of a string with another string.

When concatenating strings, it's important to consider the efficiency of the approach being used. The `+` operator may be more straightforward, but it creates a new string every time it's used, which can be inefficient for large strings. The `append()` function, on the other hand, modifies the existing string rather than creating a new one, making it a more efficient option.

## See Also:
- [C++ Strings](https://www.geeksforgeeks.org/c-strings/)
- [C++ String Manipulation](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [C++ String Concatenation](https://www.programiz.com/cpp-programming/string-concatenation)