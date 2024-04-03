---
date: 2024-01-20 17:51:16.589686-07:00
description: "Interpolering av str\xE4ngar inneb\xE4r att man blandar variabler och\
  \ str\xE4ngar f\xF6r att skapa en ny, sammansatt str\xE4ng. Programmerare interpolerar\
  \ str\xE4ngar f\xF6r\u2026"
lastmod: '2024-03-13T22:44:37.773878-06:00'
model: gpt-4-1106-preview
summary: "Interpolering av str\xE4ngar inneb\xE4r att man blandar variabler och str\xE4\
  ngar f\xF6r att skapa en ny, sammansatt str\xE4ng."
title: "Interpolera en str\xE4ng"
weight: 8
---

## Så Här Gör Du:
Java 15 (och senare) införde `String.format()` för stränginterpolering:

```java
public class StringInterpolationExample {
    public static void main(String[] args) {
        String name = "Erik";
        int age = 29;
        String greeting = String.format("Hej, jag heter %s och är %d år gammal.", name, age);
        System.out.println(greeting);  // Output: Hej, jag heter Erik och är 29 år gammal.
    }
}
```

I nyare Java-versioner kan du även använda `Text Blocks` tillsammans med `String.format()`:

```java
public class TextBlockExample {
    public static void main(String[] args) {
        String name = "Sara";
        int year = 2023;
        String message = """
                Hej %s!
                Välkommen till %d.
                """.formatted(name, year);
        System.out.println(message);
        // Output:
        // Hej Sara!
        // Välkommen till 2023.
    }
}
```

## Fördjupning
Innan Java 15, användes konkatenering med `+` eller `StringBuilder` för att skapa sammansatta strängar. `String.format()` är en mer kraftfull metod som inte bara erbjuder interpolering utan också formatering av text, datum och nummer.

Alternativ till `String.format()` inkluderar `MessageFormat` och tredjepartsbibliotek som Apache Commons Lang `StringUtils`, men dessa kan vara overkill för enkla behov.

Interpolering genom `formatted` med `Text Blocks` är en del av projekt Amber, som syftar till att förenkla kodskrivning i Java. Med detta är kod både läsbar och kompakt, vilket minskar risken för fel och gör ändringar enklare.

## Se Även
- [String.format() JavaDoc](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#format(java.lang.String,java.lang.Object...))
- [JEP 378: Text Blocks (Final)](https://openjdk.java.net/jeps/378)
- [Apache Commons Lang StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)
