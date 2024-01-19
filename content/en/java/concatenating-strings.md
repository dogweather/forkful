---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/concatenating-strings.md"
---

{{< edit_this_page >}}

# String Concatenation in Java: A Simple Guide 

## What & Why?

String concatenation in Java is about sticking two or more strings together to form one. Programmers use this to combine data, like creating a dynamic message or assembling a file path.

## How To:

Concatenation is straightforward in Java. See the simple case below:
```Java
String part1 = "Hello, ";
String part2 = "world!";
String message = part1 + part2;
System.out.println(message);  // Outputs: Hello, world!
```
With this, we've combined `part1` and `part2` into `message` using `+`.

Another common way is to use the `concat()` method from the `String` class. Here's an example:
```Java
String part1 = "Hello, ";
String part2 = "world!";
String message = part1.concat(part2);
System.out.println(message);  // Outputs: Hello, world!
```
That's it. Easy, right?

## Deep Dive

Historically, string concatenation used to be a costly operation because `String` objects in Java are immutable — each operation creates a new `String` object. But with the introduction of `StringBuilder` and `StringBuffer`, the cost is significantly reduced.

Still, if performance is a concern (like in a big loop), it's better to use `StringBuilder` or `StringBuffer`:
```Java
StringBuilder sb = new StringBuilder();
sb.append("Hello, ");
sb.append("world!");
String message = sb.toString();
System.out.println(message);  // Outputs: Hello, world!
```
This code doesn't create a new `String` instance in each operation but fills up the `StringBuilder` until it's converted back to a `String`.

Another point: while `+` and `concat()` can serve most of your needs, these approaches can fall short while dealing with `null`. `+` automatically converts `null` to "null", but `concat()` throws `NullPointerException`. Be cautious according to your use case.

## See Also

1. Official Oracle docs about [`String`](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html) class.
2. More on [`StringBuilder`](https://docs.oracle.com/javase/7/docs/api/java/lang/StringBuilder.html) and [`StringBuffer`](https://docs.oracle.com/javase/7/docs/api/java/lang/StringBuffer.html).
3. StackOverflow discussion on [String performance](https://stackoverflow.com/questions/1532461/stringbuilder-vs-string-concatenation-in-tostring-in-java) and [handling null](https://stackoverflow.com/questions/793476/error-trapping-in-java-string-concatenation).