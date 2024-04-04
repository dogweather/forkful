---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: 'Wie man es macht: .'
lastmod: '2024-04-04T01:27:46.314406-06:00'
model: gpt-4-0125-preview
summary: .
title: "Zeichen, die einem Muster entsprechen, l\xF6schen"
weight: 5
---

## Wie man es macht:
```Python
import re

# Beispiel-String
text = "Hallo, Welt! 1234"

# Alle Ziffern entfernen
keine_ziffern = re.sub(r'\d', '', text)
print(keine_ziffern)  # Ausgabe: "Hallo, Welt! "

# Satzzeichen entfernen
keine_satzzeichen = re.sub(r'[^\w\s]', '', text)
print(keine_satzzeichen)  # Ausgabe: "Hallo Welt 1234"

# Vokale entfernen
keine_vokale = re.sub(r'[aeiouAEIOU]', '', text)
print(keine_vokale)  # Ausgabe: "Hll, Wlt! 1234"
```

### Eine von mir geschriebene benutzerdefinierte Funktion

Da ich dies häufig genug mache, habe ich es in diese `delete()` Funktion refaktoriert. Es ist auch eine gute Demonstration von [Doctests](https://docs.python.org/3/library/doctest.html):

```python
def delete(zeichenkette: str, regex: str) -> str:
    """
    >>> delete("Hallo, Welt!", "l")
    'Hao, Wet!'

    >>> delete("Hallo, Welt!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", zeichenkette)
```



## Tiefere Einblicke
Die Praxis, Zeichen zu löschen, die einem Muster in einem Text entsprechen, hat tiefe Wurzeln in der Informatik, die bis zu frühen Unix-Tools wie `sed` und `grep` zurückreichen. In Python stellt das `re` Modul diese Fähigkeit bereit, wobei reguläre Ausdrücke genutzt werden – ein mächtiges und vielseitiges Werkzeug zur Textverarbeitung.

Alternativen zum `re` Modul umfassen:
- String-Methoden wie `replace()` für einfache Fälle.
- Drittanbieter-Bibliotheken wie `regex` für komplexere Muster und bessere Unicode-Unterstützung.

Unter der Haube kompiliert der Python-Interpreter das Muster bei Verwendung von `re.sub()` in eine Serie von Bytecodes, die von einer Zustandsmaschine verarbeitet werden, die das Muster-Abgleichen direkt im Eingabetext durchführt. Diese Operation kann ressourcenintensiv sein für große Zeichenketten oder komplexe Muster, deshalb sind Leistungserwägungen entscheidend für die Verarbeitung großer Datenmengen.

## Siehe auch
- [Python `re` Modul-Dokumentation](https://docs.python.org/3/library/re.html): Offizielle Dokumentation für reguläre Ausdrücke in Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/): Ein umfassender Leitfaden zu regulären Ausdrücken.
- [Real Python Tutorial zu Regex](https://realpython.com/regex-python/): Anwendungen von regulären Ausdrücken in Python im realen Leben.
