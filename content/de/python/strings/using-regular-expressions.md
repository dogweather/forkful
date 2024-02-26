---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:08.789897-07:00
description: "Regul\xE4re Ausdr\xFCcke (Regex) sind Muster, die verwendet werden,\
  \ um Kombinationen von Zeichen in Zeichenketten zu finden. Programmierer nutzen\
  \ sie f\xFCr die\u2026"
lastmod: '2024-02-25T18:49:50.568719-07:00'
model: gpt-4-0125-preview
summary: "Regul\xE4re Ausdr\xFCcke (Regex) sind Muster, die verwendet werden, um Kombinationen\
  \ von Zeichen in Zeichenketten zu finden. Programmierer nutzen sie f\xFCr die\u2026"
title: "Regul\xE4re Ausdr\xFCcke verwenden"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke (Regex) sind Muster, die verwendet werden, um Kombinationen von Zeichen in Zeichenketten zu finden. Programmierer nutzen sie für die Suche, Bearbeitung oder Manipulation von Text basierend auf definierten Mustern, was sie für Aufgaben wie Datenvalidierung, Parsen oder Transformation unverzichtbar macht.

## Wie geht das:
Die Verwendung von Regex in Python erfolgt über das Modul `re`, das eine Reihe von Funktionen zur Verarbeitung von Text mit regulären Ausdrücken bereitstellt.

### Grundlegende Mustersuche
Um nach einem Muster in einer Zeichenkette zu suchen, verwenden Sie `re.search()`. Es gibt ein Match-Objekt zurück, wenn das Muster gefunden wird, sonst `None`.
```python
import re

text = "Lerne Python-Programmierung"
match = re.search("Python", text)
if match:
    print("Muster gefunden!")
else:
    print("Muster nicht gefunden.")
```
Ausgabe:
```
Muster gefunden!
```

### Kompilieren von regulären Ausdrücken
Für die wiederholte Verwendung desselben Musters, kompilieren Sie es zuerst mit `re.compile()` für eine bessere Leistung.
```python
muster = re.compile("Python")
match = muster.search("Lerne Python-Programmierung")
if match:
    print("Kompiliertes Muster gefunden!")
```
Ausgabe:
```
Kompiliertes Muster gefunden!
```

### Zeichenketten aufteilen
Um eine Zeichenkette bei jedem Treffer eines Regex-Musters aufzuteilen, verwenden Sie `re.split()`.
```python
ergebnis = re.split("\s", "Python macht Spaß")
print(ergebnis)
```
Ausgabe:
```
['Python', 'macht', 'Spaß']
```

### Alle Treffer finden
Um alle nicht überlappenden Vorkommen eines Musters zu finden, verwenden Sie `re.findall()`.
```python
treffer = re.findall("n", "Python-Programmierung")
print(treffer)
```
Ausgabe:
```
['n', 'n']
```

### Text ersetzen
Verwenden Sie `re.sub()`, um Vorkommen eines Musters durch einen neuen String zu ersetzen.
```python
ersetzer_text = re.sub("Spaß", "großartig", "Python macht Spaß")
print(ersetzer_text)
```
Ausgabe:
```
Python ist großartig
```

### Drittanbieter-Bibliotheken
Obwohl das integrierte `re`-Modul von Python leistungsfähig ist, bieten Drittanbieter-Bibliotheken wie `regex` mehr Funktionen und verbesserte Leistung. Um `regex` zu verwenden, installieren Sie es über pip (`pip install regex`) und importieren Sie es in Ihren Code.

```python
import regex

text = "Lerne Python 3.8"
match = regex.search(r"Python\s(\d+\.\d+)", text)
if match:
    print(f"Gefundene Version: {match.group(1)}")
```
Ausgabe:
```
Gefundene Version: 3.8
```
