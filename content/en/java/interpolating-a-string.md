---
title:                "Interpolating a string"
html_title:           "Java recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpolating a string in Java is the process of combining multiple strings and variables into a single string. It allows programmers to dynamically create strings based on the values of variables, making code more versatile and efficient.

## How to:
Interpolating a string in Java is done using the `String.format()` method. Here's an example of how it works:

```java
String name = "John";
int age = 25;
String message = String.format("My name is %s and I am %d years old.", name, age);
System.out.println(message);
```

The output of the code above would be: `My name is John and I am 25 years old.` Notice how the `%s` and `%d` placeholders were replaced with the value of the `name` and `age` variables, respectively.

Interpolation also supports formatting options, such as specifying the number of decimal places for a floating point variable. Here's an example:

```java
double price = 12.50;
String message = String.format("The total cost is $%.2f", price);
System.out.println(message);
```

The output would be: `The total cost is $12.50` because we specified the `%.2f` format, which means to round the `price` variable to 2 decimal places.

## Deep Dive:
Interpolating strings has been a popular feature in programming languages since the late 1970s, with the introduction of the `printf()` function in the C programming language. Java inherited this feature and added its own implementation using the `String.format()` method.

There are also alternatives to string interpolation in Java, such as using the `StringBuilder` class or concatenating strings using the `+` operator. However, string interpolation offers a cleaner and more concise way to format strings, which is especially useful when dealing with large amounts of data.

## See Also:
- [Java String.format() documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#format(java.lang.String,%20java.lang.Object...))
- [Java StringBuilder documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/StringBuilder.html)
- [C printf() function history](https://en.wikipedia.org/wiki/Printf_format_string#History)