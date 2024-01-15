---
title:                "Die Länge eines Strings finden"
html_title:           "Python: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Länge einer Zeichenkette zu finden ist eine grundlegende Aufgabe, die in vielen Python-Programmen benötigt wird. Es ermöglicht Ihnen, die Struktur einer Zeichenkette zu verstehen und darauf basierend unterschiedliche Manipulationen durchzuführen.

## Wie geht das?

Um die Länge einer Zeichenkette zu finden, gibt es in Python eine integrierte Funktion namens `len()`. Sie wird verwendet, um die Anzahl der Zeichen in einer Zeichenkette zu zählen.

```
Python Beispiel:

s = "Hallo Welt"
print(len(s))

Ausgabe:
10
```

Wie Sie sehen können, gibt `len()` die Länge der Zeichenkette `s` als Integer-Wert zurück. Es ist wichtig zu beachten, dass Leerzeichen und Sonderzeichen auch in die Gesamtlänge einbezogen werden.

```
Python Beispiel:

s = "Python ist toll!"
print(len(s))

Ausgabe:
16
```

Die Länge kann auch für andere Datenstrukturen wie Listen oder Tupel verwendet werden. Schauen wir uns ein Beispiel an:

```
Python Beispiel:

fruits = ["Apfel", "Banane", "Orange"]
print(len(fruits))

Ausgabe:
3
```

## Tiefer einsteigen

Wenn Sie sich fragen, wie `len()` funktioniert, hier ist eine kurze Erklärung: Die Funktion durchläuft die gesamte Zeichenkette (oder das Objekt) und zählt jedes Zeichen. Am Ende wird die Gesamtzahl zurückgegeben.

Es ist auch möglich, die Länge einer Zeichenkette ohne die `len()` Funktion zu finden, indem Sie eine Schleife verwenden und jedes Zeichen manuell zählen. Diese Methode ist jedoch weniger effizient und wird in der Praxis nicht häufig verwendet.

## Siehe auch

- [Python-Dokumentation: Die `len()` Funktion](https://docs.python.org/de/3/library/functions.html#len)
- [W3Schools - Python len() Funktion](https://www.w3schools.com/python/ref_func_len.asp)