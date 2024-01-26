---
title:                "Using regular expressions"
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) are patterns used to match character combinations in text. Programmers use them for searching, editing, or manipulating strings efficiently—saving time and lines of code.

## How to:
To use regex in Java, you need `Pattern` and `Matcher` classes from `java.util.regex`. Here's an example of finding email addresses in a string.

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Contact me at hello@world.com or buzz@space.net.";
        String emailRegex = "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b";

        Pattern pattern = Pattern.compile(emailRegex);
        Matcher matcher = pattern.matcher(text);

        while (matcher.find()) {
            System.out.println(matcher.group());
        }
    }
}
```
Output:
```
hello@world.com
buzz@space.net
```

## Deep Dive
Regular expressions have been around since the 1950s, invented by mathematician Stephen Kleene. Java has integrated regex since 1.4. While powerful, regex can be overkill for simple string operations—methods like `String.contains()`, `String.split()`, and `String.startsWith()` are straightforward alternatives for basic scenarios. Under the hood, Java's regex engine (using `Pattern` and `Matcher`) compiles the pattern into a series of bytecode instructions executed by the `Matcher` against the input string.

## See Also
Explore more about regex in Java with these resources:
- [Java Pattern Class](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Java Matcher Class](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Matcher.html)
- [Oracle Java Tutorial: Regular Expressions](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Regular-Expressions.info for a deep dive into regex syntax and patterns](https://www.regular-expressions.info/)
