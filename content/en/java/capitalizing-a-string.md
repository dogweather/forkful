---
title:    "Java recipe: Capitalizing a string"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Why
Capitalizing a string may seem like a simple task, but it is an important aspect of programming. By capitalizing a string, we are ensuring consistency and proper formatting of our data, which can improve the readability and functionality of our code. In this blog post, we will explore how to capitalize a string in Java and the importance of doing so.

## How To
To capitalize a string in Java, we can use the `toUpperCase()` method from the `String` class. This method converts all the characters in a string to uppercase and returns the new string. Let's see an example of how this works:

```
// Java code to capitalize a string
String str = "programming is fun!";
String capitalizedStr = str.toUpperCase();

System.out.println(capitalizedStr);
```

The output of this code will be:

```
PROGRAMMING IS FUN!
```

As you can see, all the characters in the original string are now in uppercase. We can also use this method on individual characters to capitalize only the first letter in a string, as shown in the following example:

```
// Java code to capitalize the first letter of a string
String str = "programming is fun!";
String capitalizedStr = str.substring(0, 1).toUpperCase() + str.substring(1);

System.out.println(capitalizedStr);
```

The output of this code will be:

```
Programming is fun!
```

In this example, we are using the `substring()` method to retrieve the first character of the string, capitalizing it, and then joining it with the remaining string.

## Deep Dive
Now, let's take a deeper look at the `toUpperCase()` method. This method follows the Unicode Standard and converts all the characters in a string to their corresponding uppercase characters. This means that not all characters will be converted to uppercase in the same way. For example, the German letter "ß" will be converted to "SS" when capitalized.

It is also important to note that the `toUpperCase()` method does not modify the original string; it returns a new string with the capitalized characters. This is because strings are immutable in Java, meaning they cannot be changed once created. If you want to modify the original string, you can reassign the new capitalized string to the original variable.

## See Also
- [Java String class documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [Unicode Standard](https://unicode.org/standard/standard.html)

By following this simple guide, you now know how to capitalize a string in Java and have a better understanding of why it is important. As you continue your programming journey, remember to always pay attention to the little details, as they can make a big difference in the functionality of your code.