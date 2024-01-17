---
title:                "स्ट्रिंग को जोड़ना"
html_title:           "Java: स्ट्रिंग को जोड़ना"
simple_title:         "स्ट्रिंग को जोड़ना"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/concatenating-strings.md"
---

{{< edit_this_page >}}

#Concatenating Strings: What and Why? 
Concatenating strings simply means combining or connecting multiple strings together to create a longer string. As a programmer, this is a common task that you will encounter when working with strings. It allows you to manipulate and create new strings by combining existing ones.

#How to: 
To concatenate strings in Java, you can use the '+' operator. For example:

```Java
String greeting = "Hello";
String name = "John";
String message = greeting + name; //message = "HelloJohn"
```
You can also concatenate strings using the String.concat() method:
```Java
String fruit = "apple";
String snack = fruit.concat("s"); // snack = "apples"
```
If you want to add a space between the strings, you can use the String.format() method:
```Java
String firstName = "Jane";
String lastName = "Doe";
String fullName = String.format("%s %s", firstName, lastName); // fullName = "Jane Doe"
```

#Deep Dive: 
Concatenating strings has been a fundamental operation in programming since the early days of computing. In the past, it was done using low-level programming languages like assembly, where strings were stored as arrays of characters. However, with the advent of higher-level languages like Java, this task has become much simpler and more intuitive.

There are also alternative ways to concatenate strings, such as using the StringBuilder or StringBuffer classes. These classes offer better performance when dealing with large strings since they don't create new string objects every time the string is modified.

When concatenating strings, it is important to keep in mind potential issues like memory allocation and performance. In Java, strings are immutable, meaning they cannot be modified once created. This can lead to memory inefficiency if string concatenation is done repeatedly in a loop. In these cases, using StringBuilder or StringBuffer can be more efficient.

#See Also: 
To learn more about manipulating strings in Java, check out the official Java documentation: 
https://docs.oracle.com/javase/7/docs/api/java/lang/String.html

You can also explore alternative string concatenation methods like StringBuilder and StringBuffer in the documentation:
https://docs.oracle.com/javase/7/docs/api/java/lang/StringBuilder.html
https://docs.oracle.com/javase/7/docs/api/java/lang/StringBuffer.html