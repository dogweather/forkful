---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is a commonplace task that involves looking for a specific string (the "needle") and, optionally, replacing it with another (the "new needle"). Why? Well, it's often used for data processing, file renaming, code refactoring, and more.

## How To:

Here's the simplest way to perform search and replace in Java using the `replace()` method of the `String` class.

```Java
String original = "A stitch in time saves nine.";
String replaced = original.replace("nine", "a lot");
System.out.println(replaced);
```

Output:

```Java
A stitch in time saves a lot.
```

Take note that `replace()` is case-sensitive. For a case-insensitive version, you have to use regex with `replaceAll()` instead:

```Java
String original = "A stitch in TIME saves nine.";
String replaced = original.replaceAll("(?i)time", "life");
System.out.println(replaced);
```

Output:

```Java
A stitch in life saves nine.
```

## Deep Dive:

This method of string manipulation has been a part of Java since its inception and has origins linking back to the early days of UNIX.

Alternate methods for search and replace exist such as using a `StringBuilder` or `StringBuffer` for better performance with larger text sizes. However, the `String` version often suffices for most uses due to its simplicity.

In terms of implementation, the `replace()` method internally uses a variant of the Boyer-Moore-Horspool algorithm, while `replaceAll()` applies a regular expression match and substitution.

## See Also:

- [Oracle Java String Documentation](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html)
- [Regular Expressions in Java](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Boyer-Moore-Horspool Algorithm](https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore%E2%80%93Horspool_algorithm)