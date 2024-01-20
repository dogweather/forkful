---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Generieren von Zufallszahlen ist das Erzeugen einer Sequenz von Zahlen, die keinen erkennbaren Muster oder Beziehung aufweisen. Programmierer tun dies, um Daten zu simulieren, Tests zu randomisieren und Spiele zu kreieren.

## So geht’s:

Python stellt das Modul `random` zur Verfügung, um Zufallszahlen zu generieren.

```Python
import random

# Zufallszahl zwischen 0 und 100
print(random.randint(0, 100))
```
Bei Ausführung wäre die Ausgabe so etwas wie:
```Python
42
```
Denken Sie daran, das Ergebnis variiert bei jedem Lauf!

## Vertiefung

Zufallszahlen sind in der Informatik seit der Erfindung von Computern wichtig. Die Anwendungsbereiche sind vielfältig, von kryptographischen Anwendungen bis hin zu Simulationen und Computerspielen.

Alternativ zu Python's `random` können auch Libraries wie `numpy` mit seiner Funktion `numpy.random` verwendet werden. Der Hauptunterschied liegt darin, dass `numpy.random` für größere, mehrdimensionale Arrays optimiert ist und mehr Funktionen bietet.

Während `random` Zufallszahlen mit einer gleichmäßigen Verteilung erzeugt (d.h., jede Zahl zwischen den Grenzen hat die gleiche Wahrscheinlichkeit, ausgewählt zu werden), können Sie auch andere Verteilungen wie die Normalverteilung oder die Exponentialverteilung nutzen.

## Siehe auch:

- Die offizielle Python-Dokumentation zum `random` Modul: [https://docs.python.org/3/library/random.html](https://docs.python.org/3/library/random.html)
- Ein umfangreicher Artikel über Zufallszahlen und deren Nutzung: [https://realpython.com/python-random/](https://realpython.com/python-random/)
- Die `numpy.random` Dokumentation: [https://numpy.org/doc/stable/reference/random/index.html](https://numpy.org/doc/stable/reference/random/index.html)