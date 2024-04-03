---
date: 2024-01-26 03:46:08.581499-07:00
description: 'Wie geht das: Hier ist das Wichtigste zum Runden von Zahlen in Python.'
lastmod: '2024-03-13T22:44:53.373362-06:00'
model: gpt-4-0125-preview
summary: Hier ist das Wichtigste zum Runden von Zahlen in Python.
title: Zahlen runden
weight: 13
---

## Wie geht das:
Hier ist das Wichtigste zum Runden von Zahlen in Python:

```python
# Eine Zahl auf die nächste ganze Zahl runden
print(round(8.67))  # Ausgabe: 9

# Eine Zahl auf eine bestimmte Anzahl von Dezimalstellen runden
print(round(8.67, 1))  # Ausgabe: 8.7

# Gerade Zahlen werden abgerundet und ungerade Zahlen aufgerundet, wenn sie gleich weit entfernt sind
print(round(2.5))  # Ausgabe: 2
print(round(3.5))  # Ausgabe: 4
```

## Vertiefung
In Python ist `round()` nicht einfach nur ein Abschneiden von Dezimalstellen. Historisch gesehen folgt Python, wie viele andere Sprachen, dem "Runden zur nächsten geraden Zahl" oder "Bankers Rounding". Dies minimiert den kumulativen Fehler in Summen oder Durchschnitten, was bei Finanzberechnungen wichtig ist.

Für Alternativen gibt es `math.floor()` und `math.ceil()` aus dem Mathematik-Modul von Python, die Zahlen nach unten oder oben zur nächsten ganzen Zahl ziehen. Aber wenn es um Präzision geht, lässt dich `quantize()` des `decimal` Moduls das Rundungsverhalten spezifizieren.

Unter der Haube beschäftigt sich `round()` mit binären Fließkommazahlen. Da einige Dezimalzahlen nicht genau in Binär ausgedrückt werden können, könnte man Überraschungen erleben, wie z.B. dass `round(2.675, 2)` nicht wie erwartet `2.68` wird. Hier kommen `decimal` oder `fractions` für hohe Präzision ins Spiel.

## Siehe auch
- Pythons Dokumentation zu eingebauten Funktionen: https://docs.python.org/3/library/functions.html#round
- Dezimalzahlen mit festem Punkt und Gleitkommazahlen: https://docs.python.org/3/library/decimal.html
- Pythons Mathematik-Modul: https://docs.python.org/3/library/math.html
