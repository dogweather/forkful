---
title:                "Interpolating a string"
aliases:
- en/java/interpolating-a-string.md
date:                  2024-01-20T17:50:55.880390-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolating a string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
String interpolation lets you inject variables directly into strings. It makes code cleaner and easier to read by avoiding clunky string concatenation.

## How to:
Java introduced `String.format()` for interpolation:

```java
public class StringInterpolationExample {
  public static void main(String[] args) {
    String user = "Alice";
    int points = 1337;
    String greeting = String.format("Hi, %s! You have %d points.", user, points);
    System.out.println(greeting);
  }
}
```
Sample output:
```
Hi, Alice! You have 1337 points.
```

For more modern interpolation since Java 15, we use text blocks and `formatted()`:

```java
public class ModernStringInterpolationExample {
  public static void main(String[] args) {
    String user = "Bob";
    double accountBalance = 1234.56;
    String message = """
      Dear %s,
      Your current balance is $%.2f.
      """.formatted(user, accountBalance);
    System.out.println(message);
  }
}
```
Sample output:
```
Dear Bob,
Your current balance is $1234.56.
```

## Deep Dive
Before interpolation, Java relied on concatenation: `String greeting = "Hello, " + user + "!";`. Cumbersome and error-prone, especially as strings got complex.

Historically, languages like Perl and PHP had interpolation. Java caught up much later. `String.format()` and `PrintStream.printf()` offer similar functionality, using format specifiers that tell Java how to handle variables.

Alternatives? Besides `String.format()`, we've got `MessageFormat` and `StringBuilder`, but they're not as slick for basic interpolation. Since Java 15, text blocks simplified multi-line strings and added `formatted()` to streamline interpolation directly in place.

Implementation-wise, `String.format()` uses `Formatter`, a robust engine with many formatting options. But beware, complex strings can tank your app's performance if you're not careful.

## See Also
- [String (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Formatter (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/util/Formatter.html)
- [JEP 378: Text Blocks (Final)](https://openjdk.java.net/jeps/378)
