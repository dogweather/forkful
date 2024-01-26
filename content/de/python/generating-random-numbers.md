---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:49:39.107388-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Erzeugen von Zufallszahlen ermöglicht es Programmen, Daten oder Verhaltensweisen zu variieren, was unter anderem für Spieleentwicklung, Simulationen und Tests unerlässlich ist. Programmierer nutzen Zufallszahlen, um Ergebnisse zu diversifizieren und Realismus oder Unvorhersehbarkeit zu erzeugen.

## So geht's:
```python
import random

# Eine Zufallszahl zwischen 1 und 10
print(random.randint(1, 10))

# Eine Zufallsfließkommazahl zwischen 0 und 1
print(random.random())

# Eine zufällige Auswahl aus einer Liste
fruechte = ['Apfel', 'Banane', 'Kirsche', 'Dattel']
print(random.choice(fruechte))

# Output kann variieren
# 7
# 0.4376222438325043
# Kirsche
```

## Tiefgang:
Die Erzeugung von Zufallszahlen ist kein neues Konzept, historisch gesehen wurden physische Mittel wie Würfel oder Glücksräder verwendet. In der Informatik werden Pseudozufallszahlengeneratoren (Pseudo Random Number Generators, PRNGs) eingesetzt, weil echter Zufall schwierig digital zu erreichen ist. PRNGs nutzen Algorithmen, die zufällig wirkende Zahlenfolgen aus einem Startwert (Seed) generieren. In Python wird üblicherweise der Mersenne Twister als PRNG verwendet, eine Alternative wäre die Kryptographisch sichere Pseudozufallszahlengenerator-Bibliothek `secrets` für Zwecke, wo es um Sicherheit geht, wie das Erstellen von Passwörtern oder kryptographischen Schlüsseln.

## Siehe auch:
- Python Dokumentation für das `random` Modul: https://docs.python.org/3/library/random.html
- Python Dokumentation für das `secrets` Modul: https://docs.python.org/3/library/secrets.html
- Eine Diskussion zu Pseudozufallszahlengeneratoren vs. kryptographisch sichere Generatoren: https://stackoverflow.com/questions/20936993/how-can-i-create-a-random-number-that-is-cryptographically-secure-in-python
