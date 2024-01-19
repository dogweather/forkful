---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Java-String-Interpolation: Ein schneller Leitfaden

## Was & Warum?

String-Interpolation ist der Prozess, Variablen oder Ausdrücke in einen String einzufügen. Es ist nützlich, um dynamische Strings auf einfache und leserliche Weise zu erstellen.

## So Geht's

In Java gibt es keine eingebaute String-Interpolation, aber wir können die `String.format` Methode oder `Printf` Methode verwenden. 

```Java
String name = "World";
String greeting = String.format("Hello, %s!", name);
System.out.println(greeting); // Output: Hello, World!
```

```Java
String name = "World";
System.out.printf("Hello, %s!\n", name); // Output: Hello, World!
```

## Vertiefung

Obwohl String-Interpolation in vielen Programmiersprachen eingebaut ist, ist sie in Java nicht direkt verfügbar. Die Methode `String.format` existiert jedoch seit Java 1.5 als Alternative.

Die `String.format` und `printf` Methoden basieren auf der gleichen grundlegenden Funktionalität wie in der Sprache C.

Für komplexere Szenarien könnten Sie auch externe Bibliotheken wie Apache Commons Lang's `StrSubstitutor` oder Google's Guava verwenden.

## Siehe Auch

1. [Oracle's Java Dokumentation für String](https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/lang/String.html)
2. [Apache Commons Lang](https://commons.apache.org/proper/commons-lang/)
3. [Google's Guava Github Repository](https://github.com/google/guava)