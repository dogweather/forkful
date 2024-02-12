---
title:                "Suchen und Ersetzen von Text"
aliases:
- /de/python/searching-and-replacing-text.md
date:                  2024-01-20T17:58:38.523987-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suchen und Ersetzen von Text"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?

Suchen und Ersetzen von Text ist ein Standardvorgang, bei dem Zeichenfolgen in Daten durch andere ersetzt werden. Programmierer greifen darauf zurück, um Code zu aktualisieren, Daten zu bereinigen oder Inhalte automatisch zu modifizieren.

## So geht's:

Hier sind einfache Beispiele, wie Sie Text in Python suchen und ersetzen:

```Python
# Text suchen und ersetzen mit str.replace()
text = "Hallo Welt! Programmieren ist toll."
ersetzter_text = text.replace("toll", "super")
print(ersetzter_text)
```

Ausgabe:
```
Hallo Welt! Programmieren ist super.
```

Für komplexere Suchmuster verwenden wir das `re` Modul:

```Python
import re

# Komplexere Suchmuster mit regulären Ausdrücken
text = "Kontaktieren Sie uns unter +49 123 456789 oder unter +49 987 654321."
neuer_text = re.sub(r'\+49 (\d{3} \d{6})\d+', r'+49 \1XXX', text)
print(neuer_text)
```

Ausgabe:
```
Kontaktieren Sie uns unter +49 123 456XXX oder unter +49 987 654XXX.
```

## Tiefere Einblicke:

Die Funktionen zum Suchen und Ersetzen haben eine lange Geschichte in der Textverarbeitung und Programmierung. Der Befehl `sed` in Unix ist ein frühes Beispiel dafür. Heute gibt es viele Wege, dies in Python zu tuen. Dazu gehören einfache String-Methoden wie `str.replace()` oder das mächtigere `re` Modul für reguläre Ausdrücke.

Reguläre Ausdrücke sind eine eigene Kunstform. Sie können unglaublich nützlich sein, wenn es um komplexe Such- und Ersetzungsaufgaben geht, haben jedoch eine steilere Lernkurve.

Der `str.replace()`-Methode ist sehr einfach anzuwenden, funktioniert aber nur für einfache, direkte Ersetzungen. Für eine dynamischere Textmanipulation ist `re.sub()` des `re` Moduls der Weg, den Profis gehen - es unterstützt Mustererkennung, Gruppierung und viele andere nützliche Features.

## Siehe auch:

- Python Dokumentation für das `str`-Objekt: https://docs.python.org/3/library/stdtypes.html#string-methods
- `re` Modul Dokumentation für reguläre Ausdrücke: https://docs.python.org/3/library/re.html
- Ein Tutorial zu regulären Ausdrücken in Python: https://www.regular-expressions.info/python.html
- `sed` und Stream-Editing: https://www.gnu.org/software/sed/manual/sed.html
