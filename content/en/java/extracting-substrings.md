---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extracting Substrings in Java 

## What & Why?
Extracting substrings is about taking a portion, or subsequence, of a given string. As programmers, we do this to handle and manipulate only the data we need, improving both efficiency and specificity in our code.

## How to:

Java provides two methods, `substring(int beginIndex)` and `substring(int beginIndex, int endIndex)` to extract substrings. Here are some examples:

```Java
String str="Hello World!";

// using substring(beginIndex)
String sub1=str.substring(6);  
System.out.println(sub1);  // Outputs: World!

// using substring(beginIndex, endIndex)
String sub2=str.substring(0, 5);  
System.out.println(sub2);  // Outputs: Hello
```

## Deep Dive
Java's `substring()` became part of Java's native libraries in version 1.0, so its roots go deep. 

As an alternative, you could iterate over a string with a loop and grab characters manually, but that's slower and tedious. Using `substring()` is generally preferred because it's fast, simple, and less prone to errors.

In terms of implementation, `substring()` uses Java’s `String` class, part of Java's core libraries. It’s important to remember that Java Strings are 0-indexed, meaning counting starts from 0 not 1.

## See Also
For more detailed info, check out Oracle's official `String` API:
[Java String Documentation](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html)

For other handy `String` method assistance, see:
[Java String Methods](https://www.w3schools.com/java/java_ref_string.asp)