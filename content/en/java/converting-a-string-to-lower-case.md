---
title:                "Java recipe: Converting a string to lower case"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting strings to lower case may seem like a trivial task, but it has its own significance in Java programming. It allows for easier comparison of strings, as case sensitivity can often lead to errors. In addition, lower case strings are sometimes required for certain operations and formatting.

## How To

To convert a string to lower case in Java, we can use the `toLowerCase()` method from the String class. This method takes no arguments and returns a new string with all characters converted to lower case. Let's see an example:

```Java
String name = "JOHN";
String lowerCaseName = name.toLowerCase();
System.out.println(lowerCaseName);
```
Output:
```
john
```

We can also use the `String.valueOf()` method to convert other data types, such as integers, to strings and then convert them to lower case. Here's an example:

```Java
int number = 123;
String lowerCaseNumber = String.valueOf(number).toLowerCase();
System.out.println(lowerCaseNumber);
```
Output:
```
123
```

It is important to note that the `toLowerCase()` method only converts characters to their lower case equivalents, and does not check for accent marks or other language-specific characters. To handle these cases, we can use the `Locale` parameter in the method. Here's an example:

```Java
String city = "München";
String lowerCaseCity = name.toLowerCase(Locale.GERMAN);
System.out.println(lowerCaseCity);
```
Output:
```
münchen
```

## Deep Dive

Under the hood, the `toLowerCase()` method uses the `Character.toLowerCase()` method to convert each individual character. This method uses the Unicode standard to determine the lower case equivalent of a character. In addition, it also handles multi-character characters, such as ligatures, properly.

One thing to keep in mind is that the `toLowerCase()` method returns a new string with the converted characters, but does not change the original string. So if you need to use the converted string multiple times, it is better to store it in a new variable rather than repeatedly calling the `toLowerCase()` method.

## See Also

- [Java String documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Character documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html)