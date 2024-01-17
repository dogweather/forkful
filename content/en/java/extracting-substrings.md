---
title:                "Extracting substrings"
html_title:           "Java recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings in Java refers to the process of retrieving a smaller string from a larger string. This is a useful task for string manipulation, data validation, and data processing. Programmers often use it to extract specific information or validate user input.

## How to:

To extract substrings in Java, you can use the `substring()` method. This method takes in two parameters: the starting index and the ending index of the substring. It then returns a new string containing the characters from the original string within the specified range.

```Java
String str = "Hello World!";
String subStr = str.substring(6, 11); // Retrieves "World"
System.out.println(subStr); // Outputs "World"
```

You can also use the `length()` method to get the length of the string and use it to extract substrings from the end of the string.

```Java
String str = "Hello World!";
String subStr = str.substring(str.length() - 6); // Retrieves "World!"
System.out.println(subStr); // Outputs "World!"
```

## Deep Dive

The `substring()` method was introduced in Java 1.0 and remains an essential tool for string manipulation. It allows for more efficient and concise code compared to manually looping through the string characters. However, alternative methods such as `split()` and `StringTokenizer` can also be used for substring extraction.

Implementation-wise, the `substring()` method utilizes the `StringIndexOutOfBoundsException` class to handle invalid index inputs. It also creates a new `String` object for the extracted substring, which may result in performance issues for large strings.

## See Also

- [Java String Class Documentation](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html)
- [Official Java Tutorials: Strings](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Using String Methods in Java](https://www.tutorialspoint.com/java/java_string_substring.htm)