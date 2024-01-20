---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Länge eines Strings in Python bestimmen 

## Was & Warum?
Die Bestimmung der Länge eines Strings bedeutet, zu ermitteln, wie viele Zeichen (Einzahlen, Buchstaben, Symbole) er enthält. Es ist oft notwendig, um Abzweigungen zu kontrollieren, Schleifen zu erstellen und Fehlereingaben zu überprüfen.

## Wie geht das:
Python bietet eine eingebaute Funktion `len()`, um die Länge eines Strings zu finden:

```Python
s = 'Hallo Welt!'
print(len(s))
```

Die Ausgabe wird sein:

```Python
12
```

Beachtet bitte, dass Leerzeichen und Sonderzeichen auch als Zeichen gezählt werden!

## Tiefer eintauchen:
Die `len()`-Funktion gibt es schon seit den Anfängen von Python und hat sich als effektivste Methode zur Bestimmung der Länge eines Strings erwiesen. Sie arbeitet intern mit dem `__len__()`-Dunder-Methode, die Teil der Python-Datenmodellmethoden ist.

Es gibt zwar Alternativen, wie z.B. eine Schleife zu erstellen, um manuell jedes Zeichen in einem String zu zählen, aber in den meisten Fällen ist die `len()`-Funktion effizienter und schneller:

```Python
s = 'Hallo Welt!'
count = 0
for letter in s:
    count += 1
print(count)
```

Das liefert auch `12` als Ausgabe, ist aber mehr Arbeit und langsamer als `len()`.

## Siehe auch:
Weitere Informationen und Beispiele zur Verwendung der `len()`-Funktion finden Sie in der Python-Dokumentation: [Python len()](https://docs.python.org/3/library/functions.html#len) 
Eine saubere, detaillierte Erklärung des Python-Datenmodells (inkl. `__len__()`) ist in der Python-Dokumentation zu lesen: [Python Data Model](https://docs.python.org/3/reference/datamodel.html)