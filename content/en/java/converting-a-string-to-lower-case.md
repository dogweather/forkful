---
title:                "Java recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case may seem like a simple task, but it can actually serve an important purpose in programming. This operation can be helpful when comparing strings for equality, as it ensures that casing differences do not affect the comparison. It can also be useful when working with user input, as it allows for more flexibility in processing that input.

## How To

Converting a string to lower case in Java is a relatively straightforward process. First, you will need to create a String variable and assign it the string you want to convert. Then, you can use the `toLowerCase()` method to convert the string to lower case. Let's see an example of this in action:

```java
String str = "Hello World!";
System.out.println("Original string: " + str);
str.toLowerCase();
System.out.println("Lowercase string: " + str);
```

This code will output the following:

```
Original string: Hello World!
Lowercase string: hello world!
```

As you can see, the `toLowerCase()` method has converted the original string to all lower case letters. This method can also be used on string variables that have already been assigned, making it a versatile tool in your coding arsenal.

## Deep Dive

While converting a string to lower case may seem like a simple task, there are a few things to keep in mind. Firstly, it's important to understand that this operation does not alter the original string. Instead, it creates a new string with the converted letters. This means that you will need to assign the new string to a variable if you want to work with the converted version.

Additionally, the `toLowerCase()` method uses the default locale to perform the conversion. This means that the result may vary depending on the language and regional settings of the machine running the code. If you want to ensure consistent behavior, you can specify a specific locale as a parameter in the method.

## See Also

- [Java String Class](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [Java String toLowerCase() Method](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#toLowerCase())
- [Java Locales](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Locale.html)