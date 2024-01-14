---
title:                "Python: Unterzeichenfolgen extrahieren"
simple_title:         "Unterzeichenfolgen extrahieren"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Substring-Extraktion ist ein nützliches Konzept, das es dir ermöglicht, bestimmte Teile eines Strings zu extrahieren. Dies kann besonders hilfreich sein, wenn du mit Daten arbeitest oder nach bestimmten Mustern innerhalb eines Strings suchen möchtest.

## Wie es funktioniert

Um Substrings in Python zu extrahieren, verwenden wir die Methode `substring()` und weisen sie einer Variablen zu. Innerhalb der Klammern geben wir den Start- und Endindex an, innerhalb derer der Substring extrahiert werden soll. Zum Beispiel:

```Python
text = "Lernen ist wie rudern gegen den Strom. Hört man auf, treibt man zurück."
print(text.substring(6, 12))
```

Die Ausgabe würde "n ist w" sein, da wir die Substring-Methode verwendet haben, um die Zeichen 6 bis 12 im String auszugeben.

Manchmal kann es auch hilfreich sein, einen Teil des Strings zu extrahieren, der vor oder nach einem bestimmten Zeichen erscheint. Dafür können wir die Methode `find()` verwenden, die uns den Index des gesuchten Zeichens im String gibt. Wir können dann die Substring-Methode verwenden, um den gewünschten Teil des Strings zu extrahieren. Zum Beispiel:

```Python
text = "Python ist eine tolle Programmiersprache."
index = text.find("t")
print(text.substring(index+1))
```

Die Ausgabe wäre "t eine tolle Programmiersprache.", da wir den Teil des Strings extrahiert haben, der auf das erste "t" im ursprünglichen String folgt.

## Tiefergehende Information der Substring-Extraktion

Neben der Verwendung der `substring()`- und `find()`-Methoden gibt es noch weitere nützliche Funktionen für die Substring-Extraktion. Hier sind einige zu beachtende Punkte:

- Der Startindex für die `substring()`-Methode beginnt bei 0, während der Endindex bei 1 beginnt. Dies bedeutet, dass der Endindex den Index des Zeichens nach dem letzten zu extrahierenden Zeichen angibt.
- Du kannst auch negative Indizes verwenden, um den Substring von rechts nach links zu extrahieren. In diesem Fall beginnt der Startindex bei -1 und der Endindex bei -2.
- Wenn du den Endindex leer lässt, wird der Substring bis zum Ende des Strings extrahiert.
- Die `split()`-Methode kann auch verwendet werden, um den String in eine Liste von Substrings zu teilen, basierend auf einem bestimmten Trennzeichen.

Es gibt noch viele weitere Aspekte der Substring-Extraktion zu entdecken und zu erforschen. Versuche, verschiedene Funktionen und Methoden auszuprobieren, um ein besseres Verständnis dafür zu entwickeln.

## Siehe auch

- [String Methods in Python](https://www.w3schools.com/python/python_ref_string.asp)
- [Python 3 Documentation](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Python String Methods Cheat Sheet](https://www.cheatography.com/davechild/cheat-sheets/python-strings/)

Viel Spaß beim Extrahieren von Substrings in Python!