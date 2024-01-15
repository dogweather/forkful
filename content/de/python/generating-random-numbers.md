---
title:                "Erzeugung von Zufallszahlen"
html_title:           "Python: Erzeugung von Zufallszahlen"
simple_title:         "Erzeugung von Zufallszahlen"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt die Mühe machen, Zufallszahlen zu generieren? Nun, es gibt viele Anwendungsfälle für zufällige Daten in der Programmierung. Einige Beispiele sind die Simulation von Ereignissen, die Wettbewerbe oder Spiele nachahmen, die Erstellung von Testdaten oder die Verschlüsselung von Daten.

## Wie man Zufallszahlen generiert

Das Generieren von Zufallszahlen in Python ist sehr einfach und erfordert nur wenige Zeilen Code. Zunächst müssen wir das random-Modul importieren:

```Python
import random
```

Anschließend können wir die Funktionen des random-Moduls nutzen, um Zufallszahlen zu generieren. Hier sind einige Beispiele:

### Ganzzahlige Zufallszahlen

Um eine zufällige ganze Zahl zwischen zwei bestimmten Zahlen zu generieren, können wir `random.randint(start, end)` verwenden:

```Python
random.randint(1, 10)
# Ausgabe: 8
```

Dieser Code generiert eine zufällige ganze Zahl zwischen 1 und 10 (einschließlich 1 und 10).

### Fließkommazahlen

Wenn wir eine zufällige Fließkommazahl zwischen 0 und 1 generieren möchten, können wir `random.random()` verwenden:

```Python
random.random()
# Ausgabe: 0.536814023481
```

Um eine zufällige Fließkommazahl zwischen zwei bestimmten Zahlen zu erhalten, können wir `random.uniform(min, max)` nutzen:

```Python
random.uniform(1.5, 2.5)
# Ausgabe: 1.83428640295
```

### Zufälliges Element aus einer Liste

Mithilfe von `random.choice(list)` können wir ein zufälliges Element aus einer Liste auswählen:

```Python
fruits = ["Apfel", "Banane", "Orange", "Mango"]
random.choice(fruits)
# Ausgabe: Banane
```

### Zufällige Elemente ohne Wiederholung

Um mehrere zufällige Elemente aus einer Liste auszuwählen, ohne dass diese sich wiederholen, können wir `random.sample(list, k)` verwenden. Hier ist `k` die Anzahl der Elemente, die ausgewählt werden sollen:

```Python
fruits = ["Apfel", "Banane", "Orange", "Mango"]
random.sample(fruits, 2)
# Ausgabe: ["Mango", "Banane"]
```

## Tiefere Einblicke

Wenn es um das Generieren von Zufallszahlen geht, ist es wichtig zu verstehen, dass diese Zahlen in Wirklichkeit nicht wirklich zufällig sind. Sie basieren auf einem sogenannten "Pseudozufallszahlengenerator", der eine deterministische Reihenfolge von Zahlen verwendet, um scheinbar zufällige Ergebnisse zu erzeugen. Dieser Generator verwendet eine Startzahl, die als "Seed" bezeichnet wird, und eine festgelegte Berechnungsmethode, um die Zahlen zu generieren. Wenn dieselbe Seed-Zahl verwendet wird, wird derselbe Satz von "zufälligen" Zahlen erzeugt.

Um sicherzustellen, dass die Ergebnisse für jeden Nutzer tatsächlich unterschiedlich sind, können wir den Seed-Wert ändern, indem wir `random.seed(seed)` vor dem Aufruf einer Funktion verwenden.

## Siehe auch

- [Python-Dokumentation zu random](https://docs.python.org/3/library/random.html)
- [Random Number Generation in Python auf Real Python](https://realpython.com/python-random/)
- [Understanding Randomness in Python auf DataQuest](https://www.dataquest.io/blog/randomness-in-python/)