---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was und Warum?
Das Löschen von Zeichen, die einem Muster entsprechen, ist eine übliche Programmieraktion. Dabei werden alle Zeichen entfernt, die ein bestimmtes Muster oder Musterfolge in einer Zeichenkette erfüllen. Dies ermöglicht es Programmierern, Daten zu reinigen oder Zeichenketten zu manipulieren und so Informationen effizient zu extrahieren oder darzustellen.

## Wie funktioniert es?
In Java lässt sich dies einfach mit der Methode `replace()` oder `replaceAll()` der `String`-Klasse umsetzen. Hier ist ein einfacher Code zum Löschen aller numerischen Zeichen in einer Zeichenkette:

```Java
String str = "Hallo123 Welt456!";
str = str.replaceAll("[0-9]", "");
System.out.println(str);
```

Nach Ausführung dieses Codes würde die Ausgabe wie folgt aussehen:

```
Hallo Welt!
```

Die Zahlreichen in eckigen Klammern `[0-9]` ist ein Muster, das alle Zahlen von 0 bis 9 repräsentiert. Die Methode `replaceAll()` löscht alle Zeichen aus der Zeichenkette `str`, die diesem Muster entsprechen.

## Vertiefung
Historisch gesehen war diese Methode des Löschens von Zeichen, die einem Muster entsprechen, lange vor der Entstehung von Hochsprachen wie Java in Unix-basierten Utilities weit verbreitet. Sie stellt eine grundlegende Funktion bei der Datenmanipulation dar. Alternativen zur `replaceAll()` Methode können eigene Schleifen sein, die einzelne Char-Elemente überprüfen, obwohl dies in der Regel weniger effizient ist.

Ein wichtiger Aspekt der Implementierung besteht darin, dass die `replaceAll()` Methode in Java reguläre Ausdrücke verwendet. Diese können komplizierter sein, wenn neuere oder spezielle Zeichenmuster benötigt werden. Es ist wichtig, den Umgang mit diese regulären Ausdrücken zu verstehen, um die Methode effizient zu nutzen.

## Siehe auch
Für einen tieferen Einblick in Java-Strings und wie man mit ihnen arbeitet, können Sie die offizielle Java-Dokumentation unter [https://docs.oracle.com/javase/tutorial/java/data/strings.html](https://docs.oracle.com/javase/tutorial/java/data/strings.html) besuchen. Für den Umgang mit regulären Ausdrücken in Java und verschiedene Muster können Sie [https://docs.oracle.com/javase/tutorial/essential/regex/](https://docs.oracle.com/javase/tutorial/essential/regex/) konsultieren.