---
title:    "Java recipe: Converting a string to lower case"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Introduction: Why You Should Convert a String to Lower Case
When working with strings in Java, you may encounter instances where the case of the characters matters. For example, when comparing two strings, you may want to ignore the case and only focus on the content. In these situations, converting a string to lower case can be beneficial. It allows for easier comparison and manipulation of strings. In this blog post, we will explore how to convert a string to lower case in Java and dive deeper into the details behind this process.

## How To: Converting a String to Lower Case in Java
To convert a string to lower case in Java, we can use the `toLowerCase()` method from the `String` class. This method takes no parameters and returns a new string with all characters converted to lower case.

```Java
String str = "Hello, World!";
String lowerCaseStr = str.toLowerCase(); // lowerCaseStr = "hello, world!"
```

We can also use this method in conjunction with a string comparison to ignore case sensitivity.

```Java
String str1 = "Hello";
String str2 = "hello";
if (str1.toLowerCase().equals(str2.toLowerCase())) {
    System.out.println("The strings are equal ignoring case.");
}
```

The output would be: `The strings are equal ignoring case.`

## Deep Dive: Understanding the Process of Converting a String to Lower Case
Now, let's take a deeper look at what happens when we call the `toLowerCase()` method. When invoked, the method creates a new `char` array and loops through each character in the original string. For each character, it checks if it is an uppercase letter by using the `isUpperCase()` method from the `Character` class. If the character is uppercase, it is converted to lowercase using the `toLowerCase()` method from the `Character` class. The new lowercase character is then added to the new `char` array. Once the loop finishes, the method returns a new string created from the new `char` array.

## See Also
To learn more about string manipulation in Java, here are some helpful resources:

- [Java String Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [GeeksforGeeks: String toLowerCase() Method in Java](https://www.geeksforgeeks.org/java-lang-string-tolowercase-method-in-java/)
- [Javatpoint: String toLowerCase() Method in Java](https://www.javatpoint.com/java-string-tolowercase)