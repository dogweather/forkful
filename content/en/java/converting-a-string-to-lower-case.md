---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lowercase means transforming every uppercase letter in a string to its lowercase equivalent. Developers often do this to compare strings without considering case differences and make data case-insensitive.

## How to:

Here's the basic idea: Java's `String` class has a built-in `toLowerCase()` method. 

```Java
String str = "Hello Techies!";
str = str.toLowerCase(); // Set str to the lowercase version
System.out.println(str); // 'hello techies!'
```
Then, your program prints `hello techies!`.

Easy peasy, lemon squeezy.

Remember that strings in Java are immutable. So `toLowerCase()` doesn't change the string itself, but gives you a new one.

## Deep Dive

String lowercase conversion isn't as modern as you might think. This needs tracing back to when computers were physically massive, and upper-case-only monitors were common due to technical limitations.

Sure, `toLowerCase()` is the usual way, but you've got options:
- `StringUtils.lowerCase()`: part of Apache's Commons Lang, and it's nicely null-safe.
- `CharMatcher.javaUpperCase().replaceFrom()`: from Google's Guava, if you want to get fancy.

Under the hood, `toLowerCase()` uses `Character.toLowerCase()`. It refers to Unicode to accurately change upper case to lower case. So, it's not just for English letters. It deals with Greek, Cyrillic and others too.

## See Also

Interested about `String` class and its methods, check Java docs [here](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html).

For Apache's StringUtils, visit the [official documentation](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html).

Want to catch the Guava wave? Go [here](https://guava.dev/releases/snapshot-jre/api/docs/com/google/common/base/CharMatcher.html).