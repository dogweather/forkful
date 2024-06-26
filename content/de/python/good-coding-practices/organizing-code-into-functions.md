---
date: 2024-01-26 01:11:59.501385-07:00
description: "Wie geht das: Nehmen wir an, Sie schreiben ein Skript, um das Quadrat\
  \ und den W\xFCrfel einer Zahl zu berechnen. Ohne Funktionen ist es ein Durcheinander\
  \ von\u2026"
lastmod: '2024-03-13T22:44:53.384403-06:00'
model: gpt-4-1106-preview
summary: "Nehmen wir an, Sie schreiben ein Skript, um das Quadrat und den W\xFCrfel\
  \ einer Zahl zu berechnen."
title: Code in Funktionen organisieren
weight: 18
---

## Wie geht das:
Nehmen wir an, Sie schreiben ein Skript, um das Quadrat und den Würfel einer Zahl zu berechnen. Ohne Funktionen ist es ein Durcheinander von Wiederholungen:

```Python
zahl = 4
quadrat = zahl * zahl
wuerfel = zahl * zahl * zahl
print(f"Quadrat: {quadrat}, Würfel: {wuerfel}")

zahl = 5
quadrat = zahl * zahl
wuerfel = zahl * zahl * zahl
print(f"Quadrat: {quadrat}, Würfel: {wuerfel}")
```
Ausgabe:
```
Quadrat: 16, Würfel: 64
Quadrat: 25, Würfel: 125
```

Mit Funktionen sieht es ordentlicher aus:

```Python
def quadrat(n):
    return n * n

def wuerfel(n):
    return n ** 3

zahl = 4
print(f"Quadrat: {quadrat(zahl)}, Würfel: {wuerfel(zahl)}")

zahl = 5
print(f"Quadrat: {quadrat(zahl)}, Würfel: {wuerfel(zahl)}")
```
Ausgabe:
```
Quadrat: 16, Würfel: 64
Quadrat: 25, Würfel: 125
```

## Tiefere Einblicke
Früher, als Programme einfach waren, konnte man damit durchkommen, einfach eine Liste von Anweisungen zu schreiben. Aber als die Software komplexer wurde, erkannten die Entwickler, dass sie denselben Code immer wieder neu schrieben. Hallo, Funktionen — wiederverwendbare Blöcke von Code, die eine einzige Aktion ausführen.

Alternativen zu Funktionen sind Klassen (Bündeln von Funktionen mit den Daten, auf die sie operieren) und Inline-Code (Intelligenz genau dort, wo Sie sie brauchen, aber riskant für komplexe Aufgaben). Bei der Implementierung geht es nicht nur darum, Funktionen zu erstellen, sondern sie auch eine einzige Sache gut machen zu lassen — denken Sie an das Prinzip der einfachen Verantwortlichkeit. Funktionen sollten idealerweise auch zustandslos sein, was bedeutet, dass es keine Überraschungen mit den Daten gibt, die rein oder raus gehen.

## Siehe auch
- Die offiziellen Python-Tutorials über Funktionen: https://docs.python.org/3/tutorial/controlflow.html#defining-functions
- 'Clean Code' von Robert C. Martin, für Prinzipien, wie man saubere Funktionen schreibt.
- 'Refactoring: Verbesserung des Designs bestehender Code' von Martin Fowler, welches Beispiele für das Organisieren von Code enthält.
