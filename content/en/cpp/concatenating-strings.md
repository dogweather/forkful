---
title:                "C++ recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to combine multiple strings into one? Maybe you wanted to display a full name or create a sentence from different words. Whatever the reason, concatenating strings is a common task in programming and can be easily achieved in C++.

## How To

First, let's define what concatenating strings means. It simply refers to joining multiple strings together to create a new string. In C++, this can be done using the "<<" operator or the "append" function.

Let's look at some examples using C++ code blocks:

### Using the "<<" Operator

```C++
string firstName = "John";
string lastName = "Smith";
string fullName = firstName + " " + lastName;
cout << fullName << endl;
```

The output of this code would be "John Smith". Here, we are using the "<<" operator to combine the strings "firstName", "lastName" and a space character to create a new string called "fullName".

### Using the "append" Function

```C++
string word1 = "Programming";
string word2 = "is";
string word3 = "fun!";
string sentence = word1.append(" ").append(word2).append(" ").append(word3);
cout << sentence << endl;
```

This code would produce the output "Programming is fun!". Here, we are using the "append" function to add spaces between the different words and create a sentence.

## Deep Dive

In C++, strings are considered objects and have their own functions and properties. When using the "append" function, we are actually modifying the calling string object itself by adding the specified string to the end of it. This is different from using the "<<" operator, which creates a new string object.

It is also worth mentioning that concatenating strings can be resource intensive, especially when dealing with large strings or a large number of strings. In those cases, it is recommended to use the "stringstream" class for better performance.

## See Also

- [C++ string concatenation - Tutorialspoint](https://www.tutorialspoint.com/cplusplus/cpp_string_concatenation.htm)
- [C++ stringstream - GeeksforGeeks](https://www.geeksforgeeks.org/c-stringstream-class-applications/)
- [C++ String Class Reference - CppReference](https://en.cppreference.com/w/cpp/string/basic_string)