---
title:                "String in Großbuchstaben umwandeln"
date:                  2024-01-19
simple_title:         "String in Großbuchstaben umwandeln"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
In Python bedeutet das Großschreiben eines Strings, jeden Buchstaben in Großbuchstaben zu verwandeln. Das ist nützlich für die Benutzeroberfläche, um Titel oder wichtige Informationen hervorzuheben.

## How to:
Um einen String in Python zu großzuschreiben, gibt es mehrere Methoden. Hier sind zwei gängige Ansätze:

```python
# Methode 1: Die upper()-Funktion
text = "python programmieren macht spaß!"
print(text.upper())

# Methode 2: List Comprehension und join()
text = "python programmieren ist cool!"
capitalized_text = ''.join([char.upper() for char in text])
print(capitalized_text)
```
Ausgabe:
```
PYTHON PROGRAMMIEREN MACHT SPAß!
PYTHON PROGRAMMIEREN IST COOL!
```

## Deep Dive
Strings großzuschreiben ist ein Konzept, das so alt wie die Programmierung selbst ist. In Python benutzen wir Funktionen wie `upper()`, weil sie effizient und eingebaut sind. 

### Alternativen:
- Module wie `string` bieten Funktionen wie `string.capwords()`, um jedes Wort in einem String zu großzuschreiben.
- Die Methode `capitalize()` macht nur den ersten Buchstaben groß.

### Implementierungsdetails:
- `upper()` betrachtet die Locale-Einstellungen nicht. Es könnte bei internationalen Skripten zu Problemen führen.
- List Comprehension ist nützlich, wenn du mehr Kontrolle über den Prozess brauchst, zum Beispiel um nur bestimmte Zeichen zu ändern.

## See Also
- Python Dokumentation für Strings: https://docs.python.org/3/library/stdtypes.html#string-methods
- W3Schools Anleitung zu Python Strings: https://www.w3schools.com/python/python_strings.asp
- Python-Modul `string`: https://docs.python.org/3/library/string.html
