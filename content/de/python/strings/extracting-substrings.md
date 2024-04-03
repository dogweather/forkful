---
date: 2024-01-20 17:46:18.649037-07:00
description: "Das Herausziehen von Teilstrings (Substrings) hilft dabei, spezifische\
  \ Daten aus gr\xF6\xDFeren Textmengen zu gewinnen. Programmierer nutzen diese Technik\u2026"
lastmod: '2024-03-13T22:44:53.367236-06:00'
model: gpt-4-1106-preview
summary: "Das Herausziehen von Teilstrings (Substrings) hilft dabei, spezifische Daten\
  \ aus gr\xF6\xDFeren Textmengen zu gewinnen."
title: Teilstrings extrahieren
weight: 6
---

## What & Why?
Das Herausziehen von Teilstrings (Substrings) hilft dabei, spezifische Daten aus größeren Textmengen zu gewinnen. Programmierer nutzen diese Technik häufig für die Datenaufbereitung, Validierung oder um bestimmte Muster und Informationen zu isolieren.

## How to:
Hier ist der schnelle Weg, Substrings in Python zu extrahieren:

```python
text = "Hallo, Welt!"
teil = text[7:12]
print(teil)  # Das druckt 'Welt'
```

Noch ein Beispiel mit einem Start- und ohne Endindex: 

```python
text = "Python Programmierung"
teil = text[7:]
print(teil)  # Ergebnis: 'Programmierung'
```

So klappt's auch von hinten (negative Indizes):

```python
text = "Python Programmierung"
teil = text[-13:-1]
print(teil)  # Ergebnis: 'Programmierun'
```

Und wenn du einen Teilstring brauchst, der jeden zweiten Buchstaben nimmt:

```python
text = "Python Programmierung"
teil = text[::2]
print(teil)  # Ergebnis: 'Pto rgamirg'
```

## Deep Dive
Teilstring-Extraktion ist eine grundlegende Technik, die in vielen Programmiersprachen vorhanden ist. In Python gibt's das schon seit den Anfängen. Python nutzt eine 0-basierte Indexierung, was bedeutet, dass der erste Buchstabe an Position 0 ist. Das Substring-Slicing arbeitet mit einem `[start:stop:step]`-Modell.

Es gibt Alternativen zu Slicing, wie die `substring`-Methode in anderen Sprachen, aber in Python ist Slicing geschmeidig und effektiv. Intern verwendet Python Objekte vom Typ `slice` für diese Operationen, was zu schnellen und speichereffizienten Ergebnissen führt.

Noch tiefer? Python speichert lange Strings intern als Arrays von Zeichen. Wenn du einen Substring extrahierst, erzeugt Python ein neues String-Objekt, das einen Verweis auf einen Teil des ursprünglichen Arrays enthält. Das ist Teil der Magie, die Python bei Substring-Operationen so schnell macht.

## See Also
- Die offizielle Python-Dokumentation über die Stringtypen (auf Englisch): https://docs.python.org/3/library/stdtypes.html#string-methods
- Ein Python-Tutorial zum Thema Strings und Slicing (auf Englisch): https://realpython.com/python-strings/
- Das Python-Glossar, für grundlegende Begriffe und Konzepte (auf Englisch): https://docs.python.org/3/glossary.html
