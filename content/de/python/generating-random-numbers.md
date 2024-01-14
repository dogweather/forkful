---
title:    "Python: Generieren von Zufallszahlen"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Generieren von zufälligen Zahlen ist eine wichtige Funktion in der Programmierung. Es ermöglicht uns, verschiedene Szenarien zu simulieren und Tests durchzuführen, die auf zufällige Ereignisse basieren. Außerdem wird es oft verwendet, um Daten zu verschleiern oder um verschiedene Entscheidungen zu treffen.

## Wie funktioniert es

Die Generierung von zufälligen Zahlen in Python ist einfach und schnell. Wir können das `random`-Modul verwenden, das bereits in der Standardbibliothek enthalten ist.

Zunächst müssen wir das `random`-Modul importieren:

```Python
import random
```

Dann können wir die `random`-Funktion verwenden, um eine zufällige Gleitkommazahl zwischen 0 und 1 zu erzeugen:

```Python
randomNumber = random.random()
print(randomNumber)
```

Die Ausgabe könnte zum Beispiel `0.346982` sein.

Um eine ganze Zahl zu erhalten, können wir die `randint`-Funktion verwenden, die zwei Parameter annimmt, den Startwert und den Endwert (einschließlich). Zum Beispiel:

```Python
randomNumber = random.randint(1, 10)
print(randomNumber)
```

Die Ausgabe könnte eine ganze Zahl zwischen 1 und 10 sein, zum Beispiel `7`.

Es gibt auch viele weitere Funktionen im `random`-Modul, wie zum Beispiel `choice`, um zufällig Elemente aus einer Liste auszuwählen, oder `shuffle`, um die Reihenfolge von Elementen in einer Liste zu mischen. Eine vollständige Liste der Funktionen und ihre Verwendung finden Sie in der offiziellen Dokumentation des `random`-Moduls.

## Tiefer Einblick

Der Prozess der Generierung von zufälligen Zahlen basiert auf sogenannten Pseudorandom-Nummerngeneratoren oder PRNGs. Diese Algorithmen nutzen eine Menge von mathematischen Formeln oder Operationen, um vermeintlich zufällige Zahlen zu erzeugen, aber in Wirklichkeit laufen sie in einem vorhersagbaren Muster ab. Aus diesem Grund werden sie als "pseudo" bezeichnet.

Es ist wichtig zu beachten, dass PRNGs nicht wirklich zufällig sind und nicht für kryptografische Zwecke verwendet werden sollten, da sie leicht durch Analyse des Algorithmus reproduziert werden können. Für die meisten Anwendungsfälle sind sie jedoch ausreichend.

## Siehe auch

- Die offizielle Dokumentation des `random`-Moduls: https://docs.python.org/3/library/random.html
- Ein Tutorial zur Generierung von zufälligen Zahlen in Python: https://realpython.com/python-random/
- Erklärung der Unterschiede zwischen Zufälligkeit und Unvorhersehbarkeit: https://www.cs.auckland.ac.nz/~pgut001/pubs/random.pdf