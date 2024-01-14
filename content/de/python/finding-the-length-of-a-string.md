---
title:                "Python: Die Länge eines Strings finden"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings ist eine häufige Aufgabe in der Programmierung. Es kann nützlich sein, um Texte zu validieren, zu formatieren oder zu manipulieren, je nach den Anforderungen eines Projekts.

## Wie man die Länge eines Strings findet

Um die Länge eines Strings in Python zu finden, gibt es die integrierte Funktion `len()`. Diese Funktion akzeptiert einen String als Argument und gibt die Anzahl der Zeichen im String zurück.

Beispielcode:

```Python
text = "Hallo Welt!"
print(len(text))
```
Output: 11

Es ist auch möglich, die Funktion auf einer Eingabe vom Benutzer anzuwenden:

```Python
text = input("Gib einen Text ein: ")
print(len(text))
```

Output: Die Anzahl der eingegebenen Zeichen des Benutzers.

## Tiefere Einblicke

In Python wird die Länge eines Strings durch die Anzahl der darin enthaltenen Zeichen definiert. Dies beinhaltet auch Leerzeichen und Sonderzeichen.

Die `len()` Funktion funktioniert auch auf anderen Datenstrukturen wie Listen und Tupeln. In diesen Fällen gibt sie die Anzahl der Elemente in der jeweiligen Datenstruktur zurück.

Es ist wichtig zu beachten, dass die Funktion `len()` nur auf einzeiligen Strings funktioniert. Um die Anzahl der Zeilen in einem mehrzeiligen String zu finden, muss `splitlines()` verwendet werden.

## Siehe auch

- [Offizielle Python-Dokumentation zu len()](https://docs.python.org/de/3/library/functions.html#len)
- [Tutorial zu Strings in Python](https://www.python-kurs.eu/python3_strings.php)
- [Weitere Python-Ressourcen](https://www.python.org/)