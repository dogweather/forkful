---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

String interpolation refers to substituting variables in a string. Developers do it to incorporate dynamic values or variables into strings without the hassle of concatenating or manually formatting them.

## How to:

In Java, you can interpolate strings using `String.format()`, like this:
```Java
String name = "John";
String greeting = String.format("Hello, %s!", name);
System.out.println(greeting);
```
Output:
```Java
Hello, John!
```
In particular, `%s` in `"Hello, %s!"` is a placeholder for `name`. You may use more placeholders for more variables:
```Java
String name = "John";
String city = "London";
String introduction = String.format("%s lives in %s.", name, city);
System.out.println(introduction);
```
Output:
```Java
John lives in London.
```
## Deep Dive

String interpolation came from old programming languages like Perl and became popular in newer languages like Ruby, Python, or JavaScript. Unfortunately, Java doesn't support string interpolation natively or as smoothly.

But Java does have alternatives. Aside from `String.format()`, you can use `printf()` which is similar and more known from C++:
```Java
String name = "John";
System.out.printf("Hello, %s!\n", name);
```
Output:
```Java
Hello, John!
```
Note that `\n` is to move to the next line.

As for implementation details, `String.format()` and `printf()` use Java's Formatter class under the hood. This class interprets the format specifiers (like `%s` for a string) and does the interpolation.

## See Also

Java uses Formatter for more than just interpolating strings. Learn more about it [here from the official Java documentation](https://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html). 

For a thorough guide on format specifiers that you can use in `String.format()` or `printf()`, check out [this guide from Baeldung](https://www.baeldung.com/java-printstream-printf).