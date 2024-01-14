---
title:                "Python: Erzeugung von zufälligen Zahlen"
simple_title:         "Erzeugung von zufälligen Zahlen"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung gibt es oft Situationen, in denen man zufällige Zahlen benötigt. Beispielsweise kann man damit bestimmte Testfälle erstellen, Spiele entwickeln oder das Verhalten von Algorithmen testen. Ohne zufällige Zahlen wäre es sehr schwierig, realitätsnahe Szenarien zu simulieren. Zum Glück bietet Python eine Vielzahl von Funktionen, um zufällige Zahlen zu generieren.

## How To

Python bietet mehrere Möglichkeiten, um zufällige Zahlen zu erzeugen. Eine der einfachsten Methoden ist die Verwendung der Random-Bibliothek. Diese Bibliothek enthält eine Vielzahl von Funktionen, die es ermöglichen, Zufallszahlen auf unterschiedliche Weise zu erzeugen. Im Folgenden ist ein Beispiel für die Generierung einer zufälligen Ganzzahl zwischen 1 und 10:

```Python
import random

# Zufällige Ganzzahl zwischen 1 und 10 erzeugen
number = random.randint(1, 10)

print(number) # Output: Eine zufällige Ganzzahl zwischen 1 und 10
``` 

Man kann auch eine Liste von zufälligen Zahlen erstellen. Dafür kann man die `random.sample()` Funktion verwenden. Hier ist ein Beispiel, das eine Liste mit 5 zufälligen Dezimalzahlen zwischen 0 und 1 erstellt:

```Python
import random

# Liste mit 5 zufälligen Dezimalzahlen zwischen 0 und 1 erstellen
numbers = random.sample(range(0, 1), 5)

print(numbers) # Output: Eine Liste mit 5 zufälligen Dezimalzahlen zwischen 0 und 1
```

## Deep Dive

Die Random-Bibliothek in Python verwendet den Mersenne-Twister-Algorithmus, um zufällige Zahlen zu erzeugen. Dieser Algorithmus ist sehr schnell und erzeugt Zahlen mit einer sehr guten Gleichverteilung. Wenn man jedoch kryptographisch sichere Zufallszahlen benötigt, sollte man die `secrets` Bibliothek verwenden, die Funktionen zur Erzeugung von kryptographisch sicheren Zufallszahlen bietet.

Zusätzlich bietet Python auch die `numpy` Bibliothek, die viele Funktionen zur Erzeugung von zufälligen Zahlen enthält. Diese Bibliothek wird oft verwendet, um große Mengen von Zufallszahlen zu generieren, was besonders für wissenschaftliche Berechnungen und statistische Analysen relevant ist.

## Siehe auch

- [Die Random-Bibliothek in Python](https://docs.python.org/3/library/random.html)
- [Kapitel über Zufallszahlen in der Python Dokumentation](https://docs.python.org/3/tutorial/random.html)
- [Die numpy Bibliothek in Python](https://numpy.org/doc/stable/index.html)