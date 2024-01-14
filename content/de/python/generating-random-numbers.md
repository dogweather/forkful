---
title:                "Python: Erzeugung von Zufallszahlen"
programming_language: "Python"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Generieren von Zufallszahlen ist ein wichtiges Konzept in der Programmierung. Es ermöglicht es uns, zufällige Ergebnisse für Spiele, Simulationen, Kryptographie und vieles mehr zu erzeugen. Es ist auch ein nützliches Werkzeug für Testing und Debugging von Code.

## Wie geht man vor

Um in Python Zufallszahlen zu generieren, können wir die eingebaute Funktion `random` verwenden. Hier ist ein Beispiel, um eine zufällige Ganzzahl zwischen 1 und 10 zu erstellen:

```Python
import random
x = random.randint(1,10)
print(x)
```

Die Ausgabe könnte zum Beispiel `7` sein. Wir können auch Zufallszahlen in einer bestimmten Spanne generieren, indem wir die Funktion `randrange` verwenden. Hier ist ein Beispiel, um eine zufällige gerade Zahl zwischen 0 und 100 zu erstellen:

```Python
import random
x = random.randrange(0,101,2)
print(x)
```

Die Ausgabe könnte zum Beispiel `54` sein, da es sich um eine gerade Zahl zwischen 0 und 100 handelt.

## Tiefendurchgang

Es gibt viele verschiedene Methoden, um Zufallszahlen in Python zu generieren. Die Funktionen `random` und `randrange` sind nur zwei Beispiele. Wir können auch Zufallszahlen mit einer bestimmten Verteilung erzeugen, wie zum Beispiel mit der Funktion `random.gauss` für eine Gaußverteilung. Es ist auch möglich, eine Liste mit Zufallszahlen zu erstellen, indem man die Funktion `random.shuffle` verwendet.

Es gibt auch spezialisierte Module wie `numpy` und `scipy`, die zusätzliche Funktionen für die Zufallszahlenerzeugung bieten. Es ist wichtig, zu verstehen, welche Art von Zufallszahlen man benötigt, um die passende Methode auszuwählen.

## Siehe auch

- [Python Dokumentation: Zufallszahlen](https://docs.python.org/de/3/library/random.html)
- [Tutorial: Random-Zahlen in Python](https://www.digitalocean.com/community/tutorials/how-to-use-the-python-random-number-generator)
- [Numpy Dokumentation: Zufallszahlen](https://numpy.org/doc/stable/reference/random/index.html)