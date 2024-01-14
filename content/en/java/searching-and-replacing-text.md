---
title:                "Java recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
As a programmer, you may come across the need to search and replace text in your code or text files. This can be a tedious task if done manually, and it also increases the risk of human error. By using programming, you can automate this process and save time and effort.

## How To
To search and replace text in Java, follow these steps:

1. Create a `String` variable containing the text or code you want to search through.
2. Use the `replace()` method to replace the desired text with the new text. This method takes two arguments - the text to be replaced and the text to replace it with.
3. Store the updated text in a new `String` variable.
4. Print out the new `String` variable to see the replaced text.

Here's a Java code example that replaces all occurrences of "hello" with "hi" in a given string:

```Java
String text = "Hello, world! Hello, Java!";
String updatedText = text.replace("hello", "hi");
System.out.println(updatedText);
```

The output of this code would be:

```
Hi, world! Hi, Java!
```

You can also use the `replaceAll()` method to replace text using regular expressions. This is useful if you want to replace multiple occurrences of a word or if you want to replace text based on a pattern.

```Java
String text = "I love programming in Java!";
String updatedText = text.replaceAll("Java", "Python");
System.out.println(updatedText);
```

The output of this code would be:

```
I love programming in Python!
```

## Deep Dive
The `replace()` and `replaceAll()` methods are both case-sensitive. However, you can use the `equalsIgnoreCase()` and `toLowerCase()` methods to make the search case-insensitive.

It's also important to note that these methods return a new `String` object and do not modify the original one. This makes them safe to use as they do not affect the original data.

In addition to the `replace()` and `replaceAll()` methods, Java also has the `replaceFirst()` method which replaces only the first occurrence of the text. There is also a `replaceLast()` method which does not exist in Java natively, but you can implement it using regular expressions.

## See Also
- [Java API Documentation: String class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [GeeksforGeeks: String replace() method in Java](https://www.geeksforgeeks.org/string-replace-method-in-java/)
- [Java Tutorials: Regular Expressions](https://docs.oracle.com/javase/tutorial/essential/regex/)