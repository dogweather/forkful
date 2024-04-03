---
date: 2024-01-20 17:42:38.562001-07:00
description: 'How to: In Java, we often use the `String.replaceAll()` method with
  a regex pattern to delete characters. Here''s a quick example.'
lastmod: '2024-03-13T22:44:59.958301-06:00'
model: gpt-4-1106-preview
summary: In Java, we often use the `String.replaceAll()` method with a regex pattern
  to delete characters.
title: Deleting characters matching a pattern
weight: 5
---

## How to:
In Java, we often use the `String.replaceAll()` method with a regex pattern to delete characters. Here's a quick example:

```Java
public class PatternDeletionExample {
    public static void main(String[] args) {
        String originalString = "Hello, 123 World! This-is a test-string.";
        String pattern = "\\d|-"; // \d is a digit, - is a literal dash

        String cleanedString = originalString.replaceAll(pattern, "");
        System.out.println(cleanedString); // Prints: Hello,  World! This is a teststring.
    }
}
```
This code snips out digits and dashes to tidy up our string.

## Deep Dive
Way back when, folks manipulated strings without handy methods and regex. They did it the hard way, char by char, which was a pain. Then regular expressions (regex) came along, and things got a whole lot easier. Regex is a powerful pattern-matching standard used in text processing.

So why `replaceAll()`? It's part of the `String` class in Java, and since strings are everywhere, it became the go-to for pattern-based text modding. It takes two params: the regex for the pattern to nix and what to slap in its place—in our case, an empty string to delete it.

There are alternatives like `Pattern` and `Matcher` classes for more complex work. These come in handy for more nuanced tasks, like finding patterns without deleting them, or replacing them in more intricate ways.

The implementation hinges on the Java regex engine, which parses the pattern and applies it to the target string. It's a mini search-and-destroy mission for characters—find the pattern, then zap it.

## See Also
- Java `Pattern` class: [java.util.regex.Pattern](https://docs.oracle.com/javase/10/docs/api/java/util/regex/Pattern.html)
- Java `Matcher` class: [java.util.regex.Matcher](https://docs.oracle.com/javase/10/docs/api/java/util/regex/Matcher.html)
- Regex tutorial: [Regular Expressions – User Guide](https://docs.oracle.com/javase/tutorial/essential/regex/)
- `replaceAll()` method: [java.lang.String#replaceAll](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#replaceAll(java.lang.String,java.lang.String))
