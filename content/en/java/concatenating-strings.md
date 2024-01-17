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

## What & Why?

Concatenating strings in Java is the process of combining multiple strings into one, usually by using the "+" operator or the "concat()" method. Programmers use this technique to manipulate and modify strings, for example when creating dynamic text or generating log messages.

## How to:

To concatenate strings in Java, you can use the "+" operator or the "concat()" method. Let's take a look at some examples:

```Java
String str1 = "Hello";
String str2 = "world!";
String result = str1 + " " + str2;
System.out.println(result);
```
Output: Hello world!

In this example, we first declare two string variables, "str1" and "str2", and assign them the values "Hello" and "world!" respectively. Then, we use the "+" operator to concatenate the two strings with a space in between. Finally, we use the "println()" method to print out the concatenated string "Hello world!".

Alternatively, you can also use the "concat()" method to achieve the same result:

```Java
String str1 = "Hello";
String str2 = "world!";
String result = str1.concat(" ").concat(str2);
System.out.println(result);
```

The output will be the same as the previous example. The "concat()" method takes in a string as an argument and appends it to the end of the original string.

## Deep Dive:

Concatenating strings is a fundamental concept in programming and has been around since the early days of Java. Before the introduction of the "+" operator, programmers relied on the "concat()" method to combine strings. However, with the "+" operator, the code is more readable and straightforward.

There are alternative ways of concatenating strings in Java, such as using the "StringBuilder" class, which is more efficient for larger strings. The "StringBuffer" class is also an option, but it is slower than the "StringBuilder" class.

In terms of implementation, the "+" operator is translated into a "StringBuilder" object at compile time, making it efficient and fast. On the other hand, the "concat()" method creates a new "String" object every time it is called, which can affect performance in larger programs.

## See Also:

- Java String Concatenation: https://docs.oracle.com/javase/tutorial/java/data/buffers.html
- StringBuilder vs StringBuffer in Java: https://www.geeksforgeeks.org/stringbuilder-vs-stringbuffer-in-java/