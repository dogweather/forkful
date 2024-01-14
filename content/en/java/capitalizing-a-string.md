---
title:                "Java recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing strings is a fundamental task in many programming languages, including Java. It involves changing the case of the letters in a string, either to all uppercase or all lowercase. This is often necessary for user inputs, data validation, and formatting purposes.

## How To

The process of capitalizing a string in Java may seem daunting at first, but it is actually quite simple. There are two main methods for capitalizing strings: `toUpperCase()` and `toLowerCase()`. Let’s take a look at how to use these methods with some coding examples.

```Java
// Example 1: Using toUpperCase()

String name = "john doe";
String capitalized = name.toUpperCase();

System.out.println(capitalized); // Output: JOHN DOE

// Example 2: Using toLowerCase()

String city = "NEW YORK";
String lowercase = city.toLowerCase();

System.out.println(lowercase); // Output: new york
```

As you can see, these methods can easily manipulate the case of a string. They can also be used in conjunction with other string methods, such as `substring()` and `charAt()`, to capitalize specific letters or words in a string.

## Deep Dive

Behind the scenes, the `toUpperCase()` and `toLowerCase()` methods use the `String` class and its `Character` objects to convert the case of a string. This is because strings in Java are considered immutable, meaning they cannot be changed. Instead, these methods create a new string with the desired case.

Furthermore, it is important to note that these methods follow the Unicode standard for character conversion, so they can handle various languages and characters.

## See Also

For further reading and practice on capitalizing strings in Java, check out these resources:

- [Oracle Java Documentation on Strings](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [W3Schools Java Tutorial on Strings](https://www.w3schools.com/java/java_ref_string.asp)
- [GeeksforGeeks article on String manipulation in Java](https://www.geeksforgeeks.org/string-manipulation-in-java-with-examples/)

Now that you have a better understanding of how to capitalize strings in Java, you can confidently tackle this task in your own programming projects. Happy coding!