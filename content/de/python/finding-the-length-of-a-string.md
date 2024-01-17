---
title:                "Die Länge einer Zeichenkette finden"
html_title:           "Python: Die Länge einer Zeichenkette finden"
simple_title:         "Die Länge einer Zeichenkette finden"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Finden der Länge einer Zeichenkette ist eine grundlegende Aufgabe in der Programmierung. Es bedeutet einfach die Anzahl der Zeichen in einer Zeichenkette zu zählen. Programmierer verwenden dies, um zu überprüfen, ob eine Eingabe innerhalb der erwarteten Länge liegt oder um den Speicherbedarf für die Zeichenkette zu berechnen.

## Wie geht's?
Denken Sie daran, dass in Python eine Zeichenkette als eine Kette von Zeichen in einfachen oder doppelten Anführungszeichen dargestellt wird. Um die Länge einer Zeichenkette zu finden, können Sie die Funktion `len()` verwenden und die Zeichenkette als Argument übergeben.

```Python
# Beispiel 1
string = "Hallo Welt"
print(len(string))
Ausgabe: 11

# Beispiel 2
string = "Python ist toll"
print(len(string))
Ausgabe: 15
```

## Tiefer Einblick
Das Finden der Länge einer Zeichenkette ist eigentlich eine leichte Aufgabe, da sie von der `len()` Funktion in Python erledigt wird. Diese Funktion gibt die Anzahl der Zeichen in einer Zeichenkette zurück, unabhängig von der Art der Zeichen (Buchstaben, Zahlen, Sonderzeichen, etc.). Alternative Methoden zur Berechnung der Länge einer Zeichenkette sind Schleifen oder die `count()` Funktion.

## Siehe auch
- [Python Dokumentation zu `len()`](https://docs.python.org/3/library/functions.html#len)
- [Python Dokumentation zu Zeichenketten](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [GeeksforGeeks: Finding length of a string in Python](https://www.geeksforgeeks.org/python-strings-length/)