---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: 'Wie geht das: .'
lastmod: '2024-04-04T02:02:45.443502-06:00'
model: gpt-4-0125-preview
summary: .
title: "Zeichen l\xF6schen, die einem Muster entsprechen"
weight: 5
---

## Wie geht das:

```Python
import re

# Beispielstring
text = "Hallo, Welt! 1234"

# Entferne alle Ziffern
keine_ziffern = re.sub(r'\d', '', text)
print(keine_ziffern)  # Ausgabe: "Hallo, Welt! "

# Entferne Satzzeichen
keine_satzzeichen = re.sub(r'[^\w\s]', '', text)
print(keine_satzzeichen)  # Ausgabe: "Hallo Welt 1234"

# Entferne Vokale
keine_vokale = re.sub(r'[aeiouAEIOU]', '', text)
print(keine_vokale)  # Ausgabe: "Hll, Wrld! 1234"
```

### Meine benutzerdefinierte Funktion

Da ich dies häufig genug mache, habe ich es in diese einfache `delete()` Funktion umgearbeitet. Es ist auch eine gute Demonstration von [Doctests](https://docs.python.org/3/library/doctest.html):

```python
def delete(string: str, regex: str) -> str:
    """
    >>> delete("Hallo, Welt!", "l")
    'Hao, Wet!'

    >>> delete("Hallo, Welt!", "[a-z]")
    'H, W!'
    """
    return re.sub(regex, "", string)
```

## Tiefergehende Betrachtung
Die Praxis, Zeichen zu löschen, die einem Muster entsprechen, hat tiefe Wurzeln in der Informatik und führt zurück zu frühen Unix-Tools wie `sed` und `grep`. In Python bietet das Modul `re` diese Fähigkeit, wobei reguläre Ausdrücke genutzt werden – ein leistungsstarkes und vielseitiges Werkzeug zur Textverarbeitung.

Alternativen zum Modul `re` umfassen:
- String-Methoden wie `replace()` für einfache Fälle.
- Drittanbieter-Bibliotheken wie `regex` für komplexere Muster und bessere Unicode-Unterstützung.

Unter der Haube kompiliert der Python-Interpreter beim Einsatz von `re.sub()` das Muster in eine Reihe von Bytecodes, die von einer Zustandsmaschine verarbeitet werden, die das Muster-Matching direkt am Eingabetext durchführt. Diese Operation kann ressourcenintensiv sein für große Strings oder komplexe Muster, sodass Leistungsbetrachtungen für die Verarbeitung großer Datenmengen entscheidend sind.

## Siehe auch
- [Python `re` Modul-Dokumentation](https://docs.python.org/3/library/re.html): Offizielle Dokumentation für reguläre Ausdrücke in Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/): Ein umfassender Leitfaden zu regulären Ausdrücken.
- [Real Python Tutorial zu Regex](https://realpython.com/regex-python/): Anwendungen von regulären Ausdrücken in der realen Welt mit Python.
