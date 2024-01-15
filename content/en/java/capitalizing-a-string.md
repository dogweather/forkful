---
title:                "Capitalizing a string"
html_title:           "Java recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

You may have come across a situation where you need to capitalize a string in your Java program. This could be for formatting purposes or to make the string more readable for the user. In this article, we will explore how to capitalize a string in Java and why it can be useful.

## How To

To capitalize a string in Java, we can use the `toUpperCase()` method from the `String` class. This method will convert all characters in the string to uppercase and return a new string with the updated format. Let's look at a simple example:

```Java
// Define string to be capitalized
String str = "hello world";

// Capitalize string using toUpperCase()
String capitalizedStr = str.toUpperCase();

// Print output
System.out.println(capitalizedStr); // Outputs "HELLO WORLD"
```

We can also use the `substring()` method in combination with `toUpperCase()` to capitalize only the first character of the string. Check out the example below:

```Java
// Define string to be capitalized
String str = "hello world";

// Capitalize first character of string
String capitalizedStr = str.substring(0, 1).toUpperCase() + str.substring(1);

// Print output
System.out.println(capitalizedStr); // Outputs "Hello world"
```

As you can see, we used the `substring()` method to extract the first character and capitalize it, then concatenate it with the remaining characters of the string.

## Deep Dive

It is worth noting that the `toUpperCase()` method does not change the original string, but rather returns a new string with the updated format. This is because strings in Java are immutable, meaning their values cannot be changed. However, we can assign the new capitalized string to the same variable to update its value.

Additionally, the `toUpperCase()` method uses the default locale to convert the characters to uppercase. However, if we want to use a specific locale, we can pass it as a parameter to the method. For example, `str.toUpperCase(Locale.FRANCE)` will capitalize the string using the French locale.

## See Also

- [Java String documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [Oracle Java tutorial on String methods](https://docs.oracle.com/javase/tutorial/java/data/manipstrings.html)