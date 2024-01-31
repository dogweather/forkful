---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
date:                  2024-01-20T17:42:33.321964-07:00
model:                 gpt-4-1106-preview
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"

category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Löschen von Zeichen, die einem Muster entsprechen, bedeutet, bestimmte Segmente aus einem String zu entfernen, die das vorgegebene Muster erfüllen. Programmierer tun dies, um Daten zu bereinigen, Texte zu formatieren oder um nicht benötigte Informationen aus Strings zu eliminieren.

## So geht's:
In Java nutzen wir die `replaceAll()`-Methode von `String`, um alle Vorkommen, die einem regulären Ausdruck (RegEx) entsprechen, zu löschen.

```java
public class RegExDeletion {
    public static void main(String[] args) {
        String input = "Hello123 World456!";
        String pattern = "\\d+"; // Entfernt alle Ziffern
        String result = input.replaceAll(pattern, "");

        System.out.println(result); // Gibt "Hello World!" aus
    }
}
```

## Tiefgehender Einblick:
Bevor Methoden wie `replaceAll()` in Java Standard wurden, mussten Entwickler Schleifen und bedingte Anweisungen verwenden, um Zeichen zu löschen. Mit der Einführung von regulären Ausdrücken in Java 1.4 wurde dieser Prozess wesentlich vereinfacht. Alternativen zum Löschen von Patterns sind das Arbeiten mit `StringBuffer` oder `StringBuilder`, um Zeichen einzeln zu entfernen, was jedoch mehr Code erfordert. Die Verwendung von `replaceAll()` ist hinter den Kulissen effizient, da es den Java Pattern Matcher benutzt, der ein kompiliertes Muster für schnelle Operationen verwendet.

## Siehe auch:
- Oracle Java Docs zu `replaceAll()`: [https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/lang/String.html#replaceAll(java.lang.String,java.lang.String)](https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/lang/String.html#replaceAll(java.lang.String,java.lang.String))
- Java RegEx Tutorial: [https://docs.oracle.com/javase/tutorial/essential/regex/](https://docs.oracle.com/javase/tutorial/essential/regex/)
- Java Performance Tuning Guide für `String` Operationen: [https://www.javaperformancetuning.com/](https://www.javaperformancetuning.com/)
