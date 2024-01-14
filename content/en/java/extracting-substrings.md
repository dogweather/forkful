---
title:                "Java recipe: Extracting substrings"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why Extract Substrings?

Extracting substrings is a fundamental skill in Java programming that allows you to retrieve a portion of a string, or a sequence of characters, from a larger string. This can be useful for manipulating and analyzing text data, and is a common task in many applications.

## How To Extract Substrings

To extract a substring in Java, you can use the `substring()` method, which takes in two parameters: the starting index and the ending index of the substring you want to extract. Here's an example of how to use this method:

```Java
String sentence = "Hello world!";
String substring = sentence.substring(0, 5);
System.out.println(substring);
```

The output of this code would be `Hello`, as it is extracting the characters from index 0 to index 4 (remember, the ending index is excluded).

You can also specify just the starting index and the `substring()` method will extract the rest of the string from that index to the end. For example:

```Java
String sentence = "Hello world!";
String substring = sentence.substring(6);
System.out.println(substring);
```

The output of this code would be `world!`.

## Deep Dive into Substring Extraction

In Java, strings are indexed starting from 0 and the `substring()` method follows the same convention. This means that the first character of a string is at index 0 and the last character is at index `length - 1`.

Another important thing to note is that the `substring()` method returns a new string, rather than modifying the original string. This is important to keep in mind as it may affect the results of subsequent operations on the original string.

It's also worth mentioning that there are other methods for extracting substrings in Java, such as `charAt()` and `split()`. Each of these methods have their own use cases and it's important to understand their differences in order to choose the appropriate one for your specific task.

## See Also

- [Java String class documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [Java substring method on GeeksforGeeks](https://www.geeksforgeeks.org/java-lang-string-substring-java/)
- [Java String Methods tutorial on W3Schools](https://www.w3schools.com/java/java_ref_string.asp)