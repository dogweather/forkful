---
date: 2024-02-03 19:02:44.302416-07:00
description: "How to: Java's standard library doesn't provide a direct method to capitalize\
  \ entire strings in one go, but you can accomplish this with a combination of\u2026"
lastmod: '2024-03-13T22:44:59.957392-06:00'
model: gpt-4-0125-preview
summary: Java's standard library doesn't provide a direct method to capitalize entire
  strings in one go, but you can accomplish this with a combination of built-in methods.
title: Capitalizing a string
weight: 2
---

## How to:
Java's standard library doesn't provide a direct method to capitalize entire strings in one go, but you can accomplish this with a combination of built-in methods. For more sophisticated needs, third-party libraries like Apache Commons Lang offer straightforward solutions.

### Using Java's Built-in Methods
To capitalize a string without external libraries, you can split the string into words, capitalize the first letter of each, and then rejoin them. Here's a simple approach:

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText); // Outputs: "Hello, World!"
    }

    public static String capitalizeWords(String str) {
        char[] chars = str.toLowerCase().toCharArray();
        boolean found = false;
        for (int i = 0; i < chars.length; i++) {
            if (!found && Character.isLetter(chars[i])) {
                chars[i] = Character.toUpperCase(chars[i]);
                found = true;
            } else if (Character.isWhitespace(chars[i]) || chars[i]=='.' || chars[i]=='\'') { 
                found = false;
            }
        }
        return String.valueOf(chars);
    }
}
```

This code snippet converts the entire string to lowercase, then iterates through each character, capitalizing the first letter of each word. It considers spaces, periods, and apostrophes as word separators.

### Using Apache Commons Lang
The Apache Commons Lang library provides a more elegant solution with the `WordUtils.capitalizeFully()` method, which handles various edge cases and delimiters for you:

```java
// Add dependency: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = WordUtils.capitalizeFully(text);
        System.out.println(capitalizedText); // Outputs: "Hello, World!"
    }
}
```

To use this method, you'll need to add the Apache Commons Lang library to your project. This library method not only capitalizes the first letter of each word but also converts the rest of the letters in each word to lowercase, ensuring a consistent capitalization pattern throughout the string.
