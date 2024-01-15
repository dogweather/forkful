---
title:                "Finding the length of a string"
html_title:           "Java recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Understanding how to find the length of a string in Java is an essential skill for any Java programmer. It allows you to manipulate and analyze strings in your code, making tasks such as data validation and formatting much easier.

## How To

```Java
public class StringLength {
  public static void main(String[] args) {
    // declaring a sample string
    String str = "Hello World!";
    
    // using the length() method to find the length of the string
    int length = str.length();
    
    // printing the length
    System.out.println("The length of the string is: " + length);
  }
}
```

Output:
"The length of the string is: 12"

## Deep Dive

In Java, strings are objects, and they have methods that can be applied to them. The length() method is one such method that is used to find the length of a string. This method returns an int value, which represents the number of characters in the string.

It's important to note that the length of a string includes all characters, including white spaces and special characters. This means that a string with just one space will have a length of 1. Also, the length() method is case sensitive, so even uppercase and lowercase letters are counted as different characters.

## See Also

- [Java String class documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Tutorialspoint - Java String length() method](https://www.tutorialspoint.com/java/string_length.htm)
- [GeeksforGeeks - length() method in Java](https://www.geeksforgeeks.org/string-length-method-in-java-with-examples/)