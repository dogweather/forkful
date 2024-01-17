---
title:                "Zusammenführen von Zeichenketten"
html_title:           "Java: Zusammenführen von Zeichenketten"
simple_title:         "Zusammenführen von Zeichenketten"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was ist das & Warum?
Beim Programmieren in Java müssen wir oft mehrere Texte miteinander kombinieren, z.B. um einen Satz mit Variablen zu erstellen oder eine lange Nachricht zusammenzusetzen. Wir verwenden einen Prozess namens "String-Konkatenation", um diese Texte nahtlos zu verbinden und den gewünschten Output zu erhalten. Es ist ein unverzichtbares Werkzeug für Entwickler, um dynamische und vielseitige Texte zu erstellen.

## Wie geht's?
Die String-Konkatenation wird durch das "+" -Zeichen in Java ermöglicht. Wir können mehrere Strings miteinander verbinden, indem wir sie einfach hintereinander schreiben und das "+" -Zeichen zwischen ihnen platzieren. Hier ist ein Beispielcode:

```
String name = "Max";
String message = "Hallo, mein Name ist " + name + ".";
```

Der Variable "name" wird der Wert "Max" zugewiesen und dann wird der gesamte Satz in der Variable "message" gespeichert. Wenn wir nun "message" ausgeben, sehen wir: "Hallo, mein Name ist Max."

## Tiefer tauchen
Die Idee der String-Konkatenation ist nicht neu und wird in vielen Programmiersprachen verwendet. In Java gibt es jedoch eine effektivere Alternative, die sogenannte "StringBuilder"-Klasse. Sie ist speziell für die Konkatenation von Strings optimiert und kann die Leistung verbessern, wenn wir viele Texte miteinander verbinden müssen.

Ein weiterer wichtiger Aspekt bei der Benutzung von String-Konkatenation ist die Formatierung von Zahlen oder anderen Datentypen. Wir können "%" -Zeichen verwenden, um Platzhalter in unseren Texten zu erstellen und dann die entsprechenden Werte darauf zu übertragen. Hier ist ein Beispielcode:

```
int x = 5;
int y = 9;
String result = "Die Summe von %d und %d ist %d." % (x, y, x + y);
```

In diesem Beispiel wird das Ergebnis "Die Summe von 5 und 9 ist 14" sein.

## Siehe auch
Weitere Informationen zur String-Konkatenation und der "StringBuilder"-Klasse finden Sie in der offiziellen [Java-Dokumentation] (https://docs.oracle.com/javase/tutorial/java/data/buffers.html) und in zahlreichen Tutorials im Internet. Sie können auch praktische Übungen machen, um Ihr Verständnis zu vertiefen und zu festigen.