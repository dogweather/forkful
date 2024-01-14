---
title:    "Java: Die Länge eines Strings bestimmen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge einer Zeichenkette ist eine grundlegende, aber wichtige Aufgabe in der Programmierung. Es ermöglicht uns, zu prüfen, ob eine Eingabe innerhalb bestimmter Grenzen liegt, Zeichenketten zu manipulieren und vieles mehr.

## Wie geht das?

Es gibt mehrere Möglichkeiten, die Länge einer Zeichenkette in Java zu finden. Eine davon ist die Verwendung der ```length()``` Methode, die für alle String-Objekte verfügbar ist. Diese Methode gibt die Anzahl der Zeichen in der Zeichenkette zurück.

```Java
String str = "Hallo Welt";
int length = str.length();
System.out.println("Die Länge der Zeichenkette beträgt: " + length);
```

Dieser Code wird die Ausgabe ```Die Länge der Zeichenkette beträgt: 11``` erzeugen, da die Zeichenkette "Hallo Welt" aus 11 Zeichen besteht.

Eine andere Möglichkeit ist die Verwendung der ```toCharArray()``` Methode, die die Zeichenkette in ein Array von Char-Elementen konvertiert. Die Länge des Arrays gibt dann die Länge der Zeichenkette an.

```Java
String str = "Hallo Welt";
char[] charArray = str.toCharArray();
int length = charArray.length;
System.out.println("Die Länge der Zeichenkette beträgt: " + length);
```

In diesem Beispiel wird ebenfalls die Ausgabe ```Die Länge der Zeichenkette beträgt: 11``` erzeugt.

## Tieferer Einblick

Bei der ersten Methode, ```length()```, berechnet Java intern die Länge der Zeichenkette und gibt sie zurück. Bei der zweiten Methode, ```toCharArray()```, wird ein Array von Char-Elementen erstellt und die Länge dieses Arrays wird zurückgegeben. Diese Methode könnte etwas langsamer sein, da sie mehr Arbeitsschritte erfordert.

Außerdem gibt es noch die Möglichkeit, eine eigene Methode zu schreiben, um die Länge der Zeichenkette zu finden. Dies könnte nützlich sein, wenn man bestimmte Anforderungen hat, die von den eingebauten Methoden nicht erfüllt werden. Zum Beispiel könnte man eine Methode schreiben, die die Anzahl der tatsächlich sichtbaren Zeichen in einer Zeichenkette zurückgibt, indem sie Leerzeichen oder andere unsichtbare Zeichen ignoriert.

## Siehe auch

- [Das Java String-Objekt](https://www.w3schools.com/java/java_strings.asp)
- [Java String-Methoden](https://www.w3schools.com/java/java_ref_string.asp)
- [Tutorial: Java Strings](https://www.tutorialspoint.com/java/java_strings.htm)