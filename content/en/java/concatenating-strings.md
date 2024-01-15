---
title:                "Concatenating strings"
html_title:           "Java recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to combine multiple strings together to create a single string? This is where concatenation comes in handy! It allows you to easily merge strings, making your code more efficient and readable. 

## How To

To concatenate strings in Java, you can use the `+` operator or the `concat()` method. Let's see how they work with some examples:

```Java
// Using the + operator
String first = "Hello";
String second = "World";
String result = first + " " + second;
System.out.println(result); // Output: Hello World

// Using the concat() method
String first = "Java";
String second = "Programming";
String result = first.concat(" ").concat(second);
System.out.println(result); // Output: Java Programming
```

In the first example, the `+` operator is used to combine the strings with the addition of a space in between. In the second example, the `concat()` method is used, which takes in a string as a parameter and joins it to the end of the original string.

## Deep Dive

Behind the scenes, concatenation in Java works by creating a new string that contains the combined values of the original strings. This means that the original strings are not modified, ensuring that the original data remains intact. 

It's also important to note that concatenating strings can be resource-intensive, especially when dealing with large amounts of data. This is because every time a concatenation occurs, a new string object is created in memory. To optimize performance, it is recommended to use the `StringBuilder` class when concatenating a large number of strings. 

## See Also

- [Java String Class](https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/lang/String.html)
- [StringBuilder Class](https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/lang/StringBuilder.html)
- [Oracle Java Documentation](https://docs.oracle.com/en/java/javase/index.html)