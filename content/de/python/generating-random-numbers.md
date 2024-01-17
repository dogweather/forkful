---
title:                "Erzeugen von zufälligen Zahlen"
html_title:           "Python: Erzeugen von zufälligen Zahlen"
simple_title:         "Erzeugen von zufälligen Zahlen"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Erzeugen von zufälligen Zahlen ist ein häufiges Konzept in der Programmierung. Es bezieht sich auf das Erstellen von Zahlen, die zufällig ausgewählt wurden, ohne bestimmte Muster oder Vorhersehbarkeit aufzuweisen. Programmierer verwenden dieses Konzept aus verschiedenen Gründen, wie z.B. zur Erzeugung von Testdaten, zur Sicherheit oder zur Simulation von zufälligen Ereignissen.

# Wie geht das?

Um zufällige Zahlen in Python zu erzeugen, können Sie die Funktion `random.randint()` verwenden. Diese Funktion akzeptiert zwei Parameter: den Anfangs- und den Endwert des gewünschten Bereichs. Zum Beispiel erzeugt `random.randint(1, 10)` eine zufällige ganze Zahl zwischen 1 und 10. Wenn Sie eine Fließkommazahl benötigen, können Sie die Funktion `random.uniform()` verwenden, indem Sie den gewünschten Bereich als Parameter übergeben.

```python
import random

# Erzeugung einer zufälligen ganzen Zahl zwischen 1 und 10
random_num = random.randint(1, 10)
print(random_num)

# Erzeugung einer zufälligen Fließkommazahl zwischen 0 und 1
random_float = random.uniform(0, 1)
print(random_float)
```

Die Beispielausgabe könnte wie folgt aussehen:

```
7
0.489659782135
```

# Tiefer Einblick

Das Konzept des Erzeugens von Zufallszahlen gibt es schon seit langem und es wurde von verschiedenen Wissenschaftlern und Mathematikern wie Blaise Pascal, Pierre de Fermat und John von Neumann erforscht. Es gibt auch andere Möglichkeiten, zufällige Zahlen zu erzeugen, wie z.B. die Verwendung von Pseudozufallszahlengeneratoren. Diese verwenden einen bestimmten Algorithmus, um Zahlen zu erzeugen, die wie zufällige Zahlen aussehen, aber tatsächlich vorhersehbar sind. In Python können Sie den `random` Modul mit verschiedenen Funktionen verwenden, um zufällige Zahlen zu erzeugen.

# Siehe auch

- [Python Dokumentation zu `random`](https://docs.python.org/3/library/random.html)
- [Geschichte der Zufallszahlen](https://www.random.org/randomness/history-of-randomness/)