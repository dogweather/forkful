---
title:    "Java recipe: Converting a string to lower case"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Why 
Converting strings to lower case is a common task in Java programming. It allows for better consistency and compatibility when working with words and sentences in a program. 

# How To 
Converting a string to lower case in Java is a simple process. The String class provides a built-in method called "toLowerCase()" that does the job. Here is an example of how to use it:
```Java
String originalString = "HELLO WORLD";
String lowerCaseString = originalString.toLowerCase();
System.out.println(lowerCaseString);
```
The output of this code will be: "hello world".

# Deep Dive
Under the hood, the "toLowerCase()" method utilizes the Unicode Standards to convert uppercase characters to their corresponding lowercase characters. This means that it can handle not only English characters but also characters from other languages.  

It is important to note that the "toLowerCase()" method does not modify the original string, it returns a new string with the converted characters. This is because strings in Java are immutable, meaning they cannot be changed once created. 

There are also other ways to convert a string to lower case in Java, such as using the "StringBuffer" or "StringBuilder" class. However, the "toLowerCase()" method is the most efficient and recommended way to do so. 

# See Also 
- Java String class documentation: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html 
- String toLowerCase() method documentation: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--