---
title:    "Java recipe: Capitalizing a string"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why Capitalize a String?

Capitalizing a string is a common task in Java programming, especially when dealing with user input or manipulating text data. It involves converting the first letter of a string to uppercase and the rest of the letters to lowercase. This can help with consistency and readability in a program.

## How To: Capitalizing a String in Java

To capitalize a string in Java, we can use the `toUpperCase()` and `toLowerCase()` methods from the `String` class. Let's take a look at an example:

```Java
String name = "james"; // lowercase string
String capitalized = name.substring(0,1).toUpperCase() + name.substring(1).toLowerCase(); // capitalize first letter and lowercase the rest
System.out.println(capitalized);
```
The output of the code above will be: `James`

In this example, we first declared a string `name` with the value of "james". Then, we use the `substring()` method to split the string into two parts - the first letter and the rest of the string. We then use the `toUpperCase()` method on the first letter to make it uppercase, and the `toLowerCase()` method on the rest of the string to make it lowercase. Finally, we concatenate the two parts and assign the new capitalized string to the variable `capitalized`.

Another approach to capitalizing a string in Java is by using the `capitalize()` method from the `StringUtils` class in the Apache Commons library. This method takes in a string as a parameter and capitalizes the first letter. Here's an example:

```Java
import org.apache.commons.lang3.StringUtils; // import the Apache Commons library

String name = "james";
String capitalized = StringUtils.capitalize(name); // capitalize string
System.out.println(capitalized);
```
The output will be the same: `James`

## Deep Dive: Understanding the Process

In the above examples, we used a combination of methods to capitalize a string. In the first example, we used the `substring()` method to split the string, but we could also use the `charAt()` method to extract the first letter. Similarly, in the second example, we used the `capitalize()` method from the Apache Commons library, but we could also use the `toUpperCase()` method on the first letter and concatenate it with the rest of the string.

It's important to note that when dealing with non-English characters or symbols, the results may vary. Some languages have different rules for capitalization, and special characters may not be properly converted. It's always recommended to test and account for edge cases when working with string manipulation.

## See Also

- [Java String documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [Apache Commons StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)
- [ASCII and Unicode Characters](https://docs.oracle.com/cd/E57471_01/bigData.154/e57459/ascii.htm)