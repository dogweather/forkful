---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Zusammenfügen von Zeichenketten (String-Konkatenation) in Java ist die Methode zur Verknüpfung von zwei oder mehr Strings zu einem einzigen. Programmierer tun dies, um Informationen effizient auszudrücken und den Code lesbarer zu machen.

## So geht's:

In Java ist die String-Konkatenation ein einfacher Vorgang. Hier ist ein grundlegendes Beispiel:

```Java
String begin = "Guten ";
String end = "Tag!";
String gruss = begin + end;
System.out.println(gruss);
```
Dies wird ausgeben: `Guten Tag!`

StringBuilder oder StringBuffer können auch verwendet werden, um Strings effizient zu verbinden, besonders in einer Schleife:

```Java
StringBuilder builder = new StringBuilder();
for (int i = 0; i < 5; i++) {
  builder.append("Ja");
}
System.out.println(builder.toString());
```
Dies wird ausgeben: `JaJaJaJaJa`.

## Tiefere Einzelheiten:

Historisch gesehen waren String-, StringBuilder- und StringBuffer-Klassen die gängigsten Methoden zur Verbindung von Strings in Java. String-Konkatenation ist einfach zu verwenden, hat aber in größeren Loops Leistungsprobleme, da Strings in Java unveränderlich sind. Jede Konkatenation erzeugt ein neues String-Objekt, was zu einem hohen Speicherverbrauch führt.

Als Alternative können wir StringBuilder oder StringBuffer verwenden. Sie sind effizienter als String-Konkatenation, da sie String-Manipulationen ohne Erzeugung neuer Objekte ermöglichen. Der Unterschied zwischen beiden liegt in der Thread-Sicherheit: StringBuffer ist thread-sicher und daher etwas langsamer als StringBuilder.

Es ist erwähnenswert, dass der Java-Compiler StringBuilder intern zur Optimierung von String-Konkatenationen verwendet.

## Siehe auch:

Für mehr Details, überprüfen Sie die Java-Dokumentation:
- [String (Java Platform SE 8 )](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [StringBuilder (Java Platform SE 8 )](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [StringBuffer (Java Platform SE 8 )](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuffer.html)

Und erweiterte Themen:
- [Efficiency of Java "Double Brace Initialization"?](https://stackoverflow.com/questions/924285/efficiency-of-java-double-brace-initialization)
- [How do I optimally concatenate strings in Java?](https://stackoverflow.com/questions/1532461/stringbuilder-vs-string-concatenation-in-tostring-in-java)