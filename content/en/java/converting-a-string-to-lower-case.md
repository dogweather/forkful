---
title:                "Converting a string to lower case"
html_title:           "Java recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case is a common task in Java programming. It allows us to compare strings without worrying about case sensitivity and makes the code more readable.

## How To

To convert a string to lower case in Java, we can use the `toLowerCase()` method from the `String` class. Here's an example code block showing how to use this method:

```Java
String str = "Hello World";
String lowerStr = str.toLowerCase();
System.out.println(lowerStr);
```

The output of this code block will be `hello world`, with all characters in lower case. We can also use this method to directly compare two strings without worrying about case sensitivity. For example:

```Java
String str1 = "Java";
String str2 = "java";
if(str1.equalsIgnoreCase(str2)){
    System.out.println("The strings are equal");
}
```

In this code block, the `equalsIgnoreCase()` method is used to compare the two strings without considering case. This will print out `The strings are equal` even though the strings are technically not equal. 

## Deep Dive

Under the hood, the `toLowerCase()` method uses the `Character.toLowerCase()` method to convert each character of the string to lower case. This method uses the Unicode standard to determine the case of each character. This means that it will work for all languages, not just English.

It is important to note that the `toLowerCase()` method does not modify the original string. It returns a new string with all characters in lower case. So if we want to use the lower case version, we need to assign it to a new variable and use that variable further in the code.

We should also be careful when using the `equalsIgnoreCase()` method to compare strings. While it ignores case, it does not ignore other differences such as punctuation or whitespace. So we need to make sure that the strings are in the same format before using this method for comparison.

## See Also

Here are some additional resources for working with strings in Java:

- [Java String Tutorial](https://www.javatpoint.com/java-string)
- [Working with Strings in Java](https://www.codecademy.com/learn/learn-java/modules/learn-java-strings)
- [Official Java String API Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)