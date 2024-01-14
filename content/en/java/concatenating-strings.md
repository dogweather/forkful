---
title:                "Java recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

As a Java programmer, you may have come across the term "concatenating strings" in your coding journey. But why is it important to know about it? Well, string concatenation is a fundamental concept in Java and is used in many real-world applications. It is the process of combining two or more strings to form a new string. Knowing how to concatenate strings can be useful in tasks such as creating dynamic content, formatting text, and constructing URLs.

## How To
Let's dive into the code and see how we can concatenate strings in Java. First, we need to declare two string variables that we want to combine.

```Java
String greeting = "Hello";
String name = "John";
```

Next, we can use the `+` operator to concatenate the two strings and store the result in a new variable.

```Java
String message = greeting + " " + name;
```

Notice that we used the `+` operator multiple times to add spaces and combine the strings. We can also concatenate strings with variables and numerical values.

```Java
String sentence = "I have " + 3 + " pets";
```

The output of the above code would be "I have 3 pets". We can also use the `concat()` method of the `String` class to concatenate strings.

```Java
String phrase = greeting.concat(" ").concat(name);
```

The output would be the same as our previous example. Keep in mind that string concatenation is not limited to only two strings, you can concatenate as many strings as needed using the `+` operator or `concat()` method.

## Deep Dive
Now that we know how to concatenate strings, let's take a deeper dive into the concept. In Java, strings are immutable, which means they cannot be changed after being created. So, when we use the `+` operator to combine strings, a new string object is created every time. This can affect the performance of our code, especially when dealing with large amounts of data.

To avoid this, Java introduced the `StringBuilder` and `StringBuffer` classes, which are mutable and can be used for efficient string concatenation. These classes provide methods like `append()` and `insert()` for concatenation and manipulation of strings. They are mainly used in multi-threaded environments, where the `String` class is not thread-safe.

## See Also
For further reading on string concatenation in Java, check out these links:

- [Official Java documentation on string concatenation](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Java StringBuilder vs StringBuffer](https://www.journaldev.com/538/string-stringbuilder-stringbuffer-java)
- [A brief history of string concatenation in Java](https://dzone.com/articles/string-concatenation-performance-issues-in-java)

Happy coding!