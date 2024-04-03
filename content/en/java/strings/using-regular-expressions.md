---
date: 2024-02-03 19:03:04.819670-07:00
description: "How to: Java's built-in support for regex is primarily through the `Pattern`\
  \ and `Matcher` classes in the `java.util.regex` package. Here\u2019s a simple\u2026"
lastmod: '2024-03-13T22:44:59.963542-06:00'
model: gpt-4-0125-preview
summary: Java's built-in support for regex is primarily through the `Pattern` and
  `Matcher` classes in the `java.util.regex` package.
title: Using regular expressions
weight: 11
---

## How to:
Java's built-in support for regex is primarily through the `Pattern` and `Matcher` classes in the `java.util.regex` package. Here’s a simple example to find and print all occurrences of a word in a string, case insensitive:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Regex is great for parsing. Parsing with regex is powerful.";
        String wordToFind = "parsing";
        
        Pattern pattern = Pattern.compile(wordToFind, Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);
        
        while (matcher.find()) {
            System.out.println("Found '" + matcher.group() + "' at position " + matcher.start());
        }
    }
}
```

Output:
```
Found 'parsing' at position 16
Found 'Parsing' at position 31
```

For tasks like splitting strings, you can use the `String` class’s `split()` method with a regex:

```java
public class SplitExample {
    public static void main(String[] args) {
        String text = "Java,Python,Ruby,JavaScript";
        String[] languages = text.split(",");
        
        for (String language : languages) {
            System.out.println(language);
        }
    }
}
```

Output:
```
Java
Python
Ruby
JavaScript
```

When working with regex in Java, there might be cases where an external library can simplify complex tasks. One of the popular third-party libraries for working with regex in Java is `Apache Commons Lang`. It offers utilities like `StringUtils` that make some regex tasks more straightforward. Here’s how to use it to count matches of a substring:

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String text = "Regex makes text processing easier. Processing text with regex is efficient.";
        String substring = "processing";
        
        int count = StringUtils.countMatches(text, substring);
        System.out.println("'" + substring + "' appears " + count + " times.");
    }
}
```

To use Apache Commons Lang, you need to include it in your project. If you're using Maven, add this dependency to your `pom.xml`:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- Check for the latest version -->
</dependency>
```

Output:
```
'processing' appears 2 times.
```
