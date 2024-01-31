---
title:                "Interpolera en sträng"
date:                  2024-01-20T17:51:16.589686-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolera en sträng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Interpolering av strängar innebär att man blandar variabler och strängar för att skapa en ny, sammansatt sträng. Programmerare interpolerar strängar för att dynamiskt bygga text, vilket gör koden mer flexibel och läsbar.

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
