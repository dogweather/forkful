---
title:    "Java: Zusammenfügen von Zeichenfolgen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Die Konkatenation von Strings ist ein grundlegender Aspekt der Java-Programmierung und dient dazu, mehrere Zeichenfolgen zu einer einzigen zusammenzufügen. Dies kann in verschiedenen Anwendungsfällen nützlich sein, z.B. bei der Erstellung von Benutzernamen oder der Darstellung von dynamischen Textinhalten.

## So geht's

Die Konkatenation von Strings wird in Java durch den "+" Operator durchgeführt. Hier ist ein Beispielcode, der zwei Strings kombiniert und die Ausgabe auf der Konsole ausgibt:

```Java
String vorname = "Max";
String nachname = "Mustermann";
String vollständigerName = vorname + " " + nachname;
System.out.println(vollständigerName);
```

Das Ergebnis dieser Codezeilen würde "Max Mustermann" ausgeben. Beachten Sie, dass zwischen den Zeichenfolgen Leerzeichen hinzugefügt werden können, um für eine bessere Lesbarkeit zu sorgen.

## Tiefergehende Informationen

Beim Kombinieren von Strings ist es wichtig zu beachten, dass dies eine immutable Operation ist, was bedeutet, dass die ursprünglichen Strings unverändert bleiben und eine neue Zeichenfolge erstellt wird. Daher ist es ineffizient, große Mengen von Zeichenfolgen durch ständige Konkatenation zu verarbeiten, da dies zu einer großen Anzahl von ungenutzten Objekten im Speicher führen kann.

Eine effizientere Alternative ist die Verwendung der StringBuilder-Klasse, die speziell für die Manipulation von Zeichenfolgen entwickelt wurde. Sie bietet Methoden zum Hinzufügen, Entfernen oder Einfügen von Zeichenfolgen an einer beliebigen Stelle und speichert alle Änderungen in einem einzigen StringBuilder-Objekt.

## Siehe auch

- [Java String Klasse Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java StringBuilder Klasse Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [Tutorial zu Strings in Java](https://www.w3schools.com/java/java_strings.asp)