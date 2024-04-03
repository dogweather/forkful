---
date: 2024-01-20 17:38:38.900362-07:00
description: 'How to: The `String` class in Java has a nifty `toLowerCase()` method
  that does the hard work for you. Check out this simple usage.'
lastmod: '2024-03-13T22:44:59.960884-06:00'
model: gpt-4-1106-preview
summary: The `String` class in Java has a nifty `toLowerCase()` method that does the
  hard work for you.
title: Converting a string to lower case
weight: 4
---

## How to:
The `String` class in Java has a nifty `toLowerCase()` method that does the hard work for you. Check out this simple usage:

```java
public class LowerCaseExample {
    public static void main(String[] args) {
        String original = "Java ROCKS!";
        String lowerCased = original.toLowerCase();
        System.out.println(lowerCased);
    }
}
```

Output:

```
java rocks!
```

That's it. The string gets its volume turned down to a chilled-out lowercase.

## Deep Dive
Once upon a time, handling text was a tricky business. Different languages, different cases, computer systems screaming in confusion. Java, arriving on the scene in the '90s, sought to make things easier. The `toLowerCase()` method has been part of Java's `String` class since the early days.

But there's some cool stuff under the hood. You might wonder why `toLowerCase()` is even necessary. The thing is, not all cultures define "lower case" the same way. The method is locale-sensitive, using your system's default locale, or you can specify one using `toLowerCase(Locale locale)`.

Here's another twist: languages with more ornate scripts, like Turkish, have special "dotless" i characters that could throw a regular lower-casing out the window. Hence, Java provides the option to be meticulous with character conversions.

Alternatives? Sure, you could romp through the string with a `for` loop, swapping chars manually. But why reinvent the wheel when Java's got you covered?

Also, this might surprise some: strings in Java are immutable. When you `toLowerCase()`, you're not modifying the original string, you're creating a fresh new one, vest and all.

## See Also
Check out these resources to up your string game:

- Java String API: [](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- Java Locale Class: [](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Locale.html)
- Unicode Case Mappings: [](https://unicode.org/reports/tr21/)

And for the gritty details on the Unicode Standard:

- The Unicode Consortium: [](https://unicode.org/)
