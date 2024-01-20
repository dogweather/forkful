---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att interpolera en sträng innebär att sätta in variabelvärden direkt in en sträng. Programmerare gör detta för att förenkla och snabba upp strängmanipulation, samt för att göra koden renare och lättare att läsa.

## Hur man gör:

Med Java 15 kan du enkelt interpolera strängar genom att använda formaterade strängar (Formatted Strings). Här är ett exempel:

```java
String name = "Ingrid";
int age = 30;
String greeting = "Hej %s, du är %d år gammal.";
System.out.printf(greeting, name, age);
```

Output:

```
Hej Ingrid, du är 30 år gammal.
```

## Fördjupning:

Historiskt sett är stringinterpolering inte en ny idé, det har använts i årtionden av olika programmeringsspråk som Perl och Ruby. För Java, användes StringBuilder, String.format, eller concatenation innan införseln av stränginterpolation.

Stränginterpolering är lättare och renare, men det finns fortfarande andra alternativ. Du kan fortfarande använda `String.format` eller `StringBuilder` om du vill.

Notera att stränginterpolering i Java implementeras med hjälp av formaterade strängar, så det är ingen grundläggande del av språket.

## Se även:

För mer information om stränginterpolering och formaterade strängar, ta en titt på dessa resurser:

- [Oracle Java Dokumentation](https://docs.oracle.com/)
- [String Formatting in Java](https://dzone.com/articles/java-string-format-examples)
- [Java String Interpolation Tutorial](https://www.baeldung.com/java-string-interpolation)