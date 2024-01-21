---
title:                "Extracting substrings"
date:                  2024-01-20T17:45:46.817922-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracting substrings"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings means pulling out a specific part of a string—a sequence of characters inside a bigger string. Programmers slice and dice strings to validate inputs, parse data, or simply to display only the relevant bits to users.

## How to:

Extracting a substring in Java is straightforward using the `substring` method. Here's how you do it:

```java
public class SubstringExample {
    public static void main(String[] args) {
        String fullString = "Hello, World!";

        // Extract from index 7 to the end of the string
        String sub1 = fullString.substring(7);
        System.out.println(sub1); // Output: World!

        // Extract from index 0 to index 4 (5 is not included)
        String sub2 = fullString.substring(0, 5);
        System.out.println(sub2); // Output: Hello
    }
}
```

**Remember**: In Java, string indexing starts at 0.

## Deep Dive

The `substring` method has been around since the early days of Java, providing a simple way to get parts of a string. In older versions of Java, `substring` would share the original character array, which could lead to memory leaks if the original string was large and the substring was kept for a long time. Since Java 7 update 6, `substring` creates a new string, so the old one can be garbage-collected if not used elsewhere.

Also, before reaching out to `substring`, consider if you can use `split`, `replace`, or regex utilities for more complex scenarios. Internally, `substring` in Java uses methods from the `String` class that copy arrays—efficient, but not something you have direct control over.

## See Also

- For a full picture of what you can do with strings in Java, take a look at the `String` class documentation: [String (Java SE 15 & JDK 15)](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html)
- Diving into more complex string manipulation? The `Pattern` and `Matcher` classes are your friends: [Pattern (Java SE 15 & JDK 15)](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/util/regex/Pattern.html)
- A tutorial on using regular expressions in Java: [Regular Expressions](https://docs.oracle.com/javase/tutorial/essential/regex/)

Whether it's for a quick trim or a complex data extraction, the functions you need are there. Keep your toolkit well-understood and ready to go.