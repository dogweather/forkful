---
title:                "Finding the length of a string"
date:                  2024-01-20T17:47:26.190328-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finding the length of a string"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Finding a string's length means figuring out how many characters it contains. Programmers often do this to validate input, loop through characters, or align text.

## How to:
Java strings have a `length()` method. Call it, and you get the character count. Easy.

```java
public class StringLengthExample {
    public static void main(String[] args) {
        String greeting = "Hello, World!";
        int length = greeting.length();

        System.out.println("The length of the string is: " + length);
        // Output: The length of the string is: 13
    }
}
```

## Deep Dive
The `length()` method dates back to the earliest Java versions, making it a long-standing part of the `String` class. It's simple but essential. Internally, a `String` in Java is backed by a character array, with the `length()` method returning the size of this array. Crucially, Java strings are immutable, so once created, the length doesn't change, making the method quick and reliable.

Alternatives? Well, other than rolling your own function to count characters (unnecessary and non-performant), not really. Keep in mind that `length()` returns the number of `char` units, not necessarily code points. For Unicode characters that don't fit in the standard 16-bit `char` size, consider using `codePointCount()` if you need to account for supplementary characters.

## See Also
Dive deeper or explore related topics:
- [Java String Documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Java Character Class Docs](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Character.html) for understanding more on Unicode, characters, and code points.
- [Oracle's Java Tutorials](https://docs.oracle.com/javase/tutorial/java/data/strings.html) for a broader understanding of strings in Java.