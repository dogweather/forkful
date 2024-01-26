---
title:                "Ermittlung der Zeichenkettenlänge"
date:                  2024-01-20T17:48:08.923746-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ermittlung der Zeichenkettenlänge"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Länge eines Strings herauszufinden bedeutet, die Anzahl der enthaltenen Zeichen zu zählen. Programmierer tun dies, um Eingaben zu validieren, Schleifenbedingungen zu setzen oder einfach um Daten zu manipulieren.

## So geht's:
```python
text = "Hallo, Welt!"
laenge = len(text)
print(laenge)  # Ausgabe: 12
```

```python
wort = "Programmieren"
print(len(wort))  # Ausgabe: 13
```

## Tiefer eintauchen:
Die Funktion `len()` in Python gibt die Anzahl der Elemente in einem Objekt zurück. Die Leichtigkeit von `len()` ist kein Zufall, sondern eine bewusste Designentscheidung. In früheren Programmiersprachen musste man oft Schleifen konstruieren, um die Länge zu ermitteln. Python nimmt diese Last ab. Die `len()`-Funktion ist schnell und effizient, weil sie direkt auf das `__len__` Attribut des Objekts zugreift.

Alternativ könnten wir durch den String iterieren und dabei zählen, eine Methode, die in low-level Sprachen üblich ist:
```python
def string_length(s):
    length = 0
    for char in s:
        length += 1
    return length

print(string_length("Hallo, Welt!"))  # Ausgabe: 12
```
Aber das ist in Python unnötig kompliziert und nicht die bevorzugte Herangehensweise.

Es ist auch wichtig, zu erwähnen, dass `len()` in Python die tatsächliche Anzahl der Unicode-Code-Points zählt, was nicht immer gleich der sichtbaren Zeichenanzahl ist, insbesondere bei Verwendung von kombinierenden Zeichen.

## Siehe auch:
- Python Dokumentation zur `len()` Funktion: https://docs.python.org/3/library/functions.html#len
- Unicode-Standard und kombinierende Zeichen: https://unicode.org/reports/tr15/
- Python's Designphilosophie bezüglich eingebauter Funktionen: https://www.python.org/dev/peps/pep-0020/
