---
title:                "Auslesen von Teilstrings"
html_title:           "Java: Auslesen von Teilstrings"
simple_title:         "Auslesen von Teilstrings"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Extrahieren von Teilzeichenfolgen ist ein gängiger Prozess in der Java-Programmierung. Es bezieht sich auf das Extrahieren einer Teilmenge von Zeichen aus einer vorhandenen Zeichenfolge. Programmierer tun dies in der Regel, um bestimmte Informationen aus einer längeren Zeichenfolge zu isolieren oder zu manipulieren. 

## Wie geht's?
Das Extrahieren von Teilzeichenfolgen in Java ist relativ einfach und kann auf verschiedene Weisen erfolgen. Eine Möglichkeit ist die Verwendung der ```substring()``` Methode. Diese Methode nimmt zwei Parameter, den Index des Startzeichens und den Index des letzten Zeichens, die in die neu erstellte Teilzeichenfolge aufgenommen werden sollen. Wenn zum Beispiel eine Zeichenfolge "Hallo Welt" ist und wir eine Teilzeichenfolge von "Welt" extrahieren möchten, können wir folgenden Code verwenden:

```
String s = "Hallo Welt";
String sub = s.substring(6, 10);
System.out.println(sub);
```
Dies wird "Welt" als Ausgabe produzieren.

## Tieferes Eintauchen
Die ```substring()``` Methode gibt es schon seit den Anfängen von Java und war sogar in der ursprünglichen Version von Java enthalten. Es ist auch wichtig zu beachten, dass Zeichenketten in Java unveränderlich sind, was bedeutet, dass die ursprüngliche Zeichenkette nicht verändert wird, sondern eine neue Teilzeichenfolge erstellt wird. Alternativ zur Verwendung der ```substring()``` Methode gibt es auch die Möglichkeit, die ```split()``` Methode zu nutzen, die eine Zeichenfolge anhand eines bestimmten Zeichens oder Musters in ein Array von Teilzeichenfolgen aufteilen kann.

## Siehe auch
Für weitere Informationen über das Extrahieren von Teilzeichenfolgen in Java, schauen Sie sich die offizielle Dokumentation von Oracle an: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int