---
title:                "Capitalizing a string"
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string means making the first letter uppercase with the rest of the letters lowercase. Programmers use this to standardize text data, like user input or names, ensuring consistency across a dataset.

## How to:

In Java, there's no built-in method to capitalize a string fully (first letter uppercase, rest lowercase), but here's a quick function to do just that:

```java
public class StringCapitalizer {
    public static void main(String[] args) {
        String input = "java is fun!"; // example string
        String output = capitalizeString(input);
        System.out.println(output); // Java is fun!
    }

    public static String capitalizeString(String str) {
        if(str == null || str.isEmpty()) {
            return str;
        }
        return str.substring(0, 1).toUpperCase() + str.substring(1).toLowerCase();
    }
}
```

## Deep Dive

Before Java 8, the above method was a common way to capitalize a string. Since the introduction of streams in Java 8, we can also manipulate strings with greater flexibility.

An alternative way to capitalize using streams:

```java
import java.util.stream.*;

public class StringCapitalizer {
    public static void main(String[] args) {
        String input = "java is cool!";
        String output = Arrays.stream(input.split("\\s"))
                              .map(word -> word.substring(0, 1).toUpperCase() + word.substring(1).toLowerCase())
                              .collect(Collectors.joining(" "));
        System.out.println(output); // Java Is Cool!
    }
}
```

This splits the string into words, capitalizes each one, and joins them back together. Note the difference: each word is capitalized, not just the first one.

Strings are immutable in Java—meaning, once created, they can't change. Methods that seem to alter strings, like `toUpperCase` or `toLowerCase`, actually create new strings with changes applied.

Performance-wise, StringBuilder is often used for string manipulation, because it's mutable. It avoids the cost of creating multiple string objects. However, for simple capitalization, the performance gain isn't a big deal, hence a `StringBuilder` example is omitted.

## See Also

- [Java String API Docs](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Collector Docs](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html)
- [StringJoiner Docs](https://docs.oracle.com/javase/8/docs/api/java/util/StringJoiner.html)
