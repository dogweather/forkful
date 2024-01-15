---
title:                "Searching and replacing text"
html_title:           "Java recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Java:

## Why
Searching and replacing text are fundamental tasks in programming and can save you time and effort when dealing with large amounts of text. By learning how to perform these actions in Java, you can easily manipulate your data and make your code more efficient.

## How To
To perform a search and replace in Java, you can use the `replace()` method from the `String` class. Let's take a look at an example:

```Java
String text = "Hello World";
String replacedText = text.replace("Hello", "Hi");
System.out.println(replacedText);
```

This code will print out "Hi World" as it replaces the word "Hello" with "Hi" in the original string.

You can also use regular expressions to search and replace patterns within a string. The `replaceAll()` method allows you to specify a regular expression and the replacement text. Here's an example:

```Java
String text = "I love Java programming";
String replacedText = text.replaceAll("Java", "Python");
System.out.println(replacedText);
```

This code will output "I love Python programming" as it replaces all instances of "Java" with "Python" in the string.

## Deep Dive
When using the `replaceAll()` method, you have the option to use capturing groups in your regular expression. This allows you to capture specific parts of the text and use them in the replacement string. Let's take a look at an example:

```Java
String text = "Name: John, Age: 25";
String replacedText = text.replaceAll("Name: (.*), Age: (.*)", "Name: $1, Age: $2, Gender: Male");
System.out.println(replacedText);
```

In this code, we use two capturing groups to extract the name and age from the original string. The `$1` and `$2` in the replacement string refer to the first and second capturing group respectively. The output will be "Name: John, Age: 25, Gender: Male" as it adds the gender "Male" to the string.

You can also use the `StringBuffer` and `StringBuilder` classes to improve performance when doing multiple replacements in a single string. These classes provide a more efficient way to manipulate strings in Java.

## See Also
- [Java String Class](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html)
- [Java Regular Expressions](https://docs.oracle.com/javase/tutorial/essential/regex/index.html)