---
date: 2024-01-20 17:51:18.282491-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) Historically, Java wasn't big on string interpolation. We used concatenation\
  \ (`+`) or `String.format()` for dynamic strings. Java\u2026"
lastmod: '2024-04-05T22:51:02.178268-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:)\
  \ Historically, Java wasn't big on string interpolation."
title: "\u0406\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0456\u044F \u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 8
---

## How to: (Як це зробити:)
```Java
public class StringInterpolationExample {
    public static void main(String[] args) {
        String user = "Василь";
        int points = 1337;
        
        // Before Java 15, we used String.format()
        String oldWay = String.format("Привіт, %s! Ти заробив %d балів.", user, points);
        System.out.println(oldWay); // Output: Привіт, Василь! Ти заробив 1337 балів.
        
        // With Java 15, we can use formatted() directly on strings
        String newWay = "Привіт, %s! Ти заробив %d балів.".formatted(user, points);
        System.out.println(newWay); // Output: Привіт, Василь! Ти заробив 1337 балів.
        
        // And with Java 16+, we have String template literals
        // String templateLiteral = `Hello, ${user}! You've earned ${points} points.`; // Not actual Java syntax as of the last knowledge update
    }
}
```

## Deep Dive (Поглиблений Розбір)
Historically, Java wasn't big on string interpolation. We used concatenation (`+`) or `String.format()` for dynamic strings. Java 15 introduced `formatted()` for a bit cleaner code. As of now, Java doesn't have the same template literals JavaScript does, but who knows what future versions will bring?

Alternatives? Besides `format()` and `formatted()`, you can concatenate with `+`, use `StringBuilder`, or go fancy with `MessageFormat` or even external libraries like Apache's `StrSubstitutor`.

Implementation-wise, remember that strings in Java are immutable. When we "interpolate," we're actually creating new strings. Keep it in mind for memory-sensitive situations.

## See Also (Додатково)
- Oracle's official Java documentation: https://docs.oracle.com/en/java/
- A deeper dive into `String.format()`: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#format(java.lang.String,java.lang.Object...)
- Why are strings immutable in Java? https://stackoverflow.com/questions/22397861/why-is-string-immutable-in-java

Remember, keep your code clean and your curiosity fresh. Happy coding!
