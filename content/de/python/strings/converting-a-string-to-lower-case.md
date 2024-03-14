---
date: 2024-01-20 17:39:09.376917-07:00
description: "Um einen String in Kleinbuchstaben umzuwandeln, ver\xE4ndert man jeden\
  \ Gro\xDFbuchstaben in seinem \xC4quivalent in Kleinbuchstaben. Dies ist n\xFCtzlich\
  \ f\xFCr die\u2026"
lastmod: '2024-03-13T22:44:53.365386-06:00'
model: gpt-4-1106-preview
summary: "Um einen String in Kleinbuchstaben umzuwandeln, ver\xE4ndert man jeden Gro\xDF\
  buchstaben in seinem \xC4quivalent in Kleinbuchstaben. Dies ist n\xFCtzlich f\xFC\
  r die\u2026"
title: Umformung eines Strings in Kleinbuchstaben
---

{{< edit_this_page >}}

## Was & Warum?
Um einen String in Kleinbuchstaben umzuwandeln, verändert man jeden Großbuchstaben in seinem Äquivalent in Kleinbuchstaben. Dies ist nützlich für die Normalisierung von Textdaten, z.B. bei Suchoperationen oder beim Vergleich zweier Zeichenketten, um Groß- und Kleinschreibung zu ignorieren.

## So geht's:
```python
# String in Kleinbuchstaben umwandeln
text = "Hallo Welt!"
lower_text = text.lower()
print(lower_text)  # Ausgabe: hallo welt!
```

Ein Beispiel, um zu demonstrieren, wieso es praktisch ist:

```python
# Vergleich ohne Beachtung der Groß- und Kleinschreibung
nutzer_input = "Morgenstund hat Gold im Mund."
suchwort = "MORGENSTUND"

if nutzer_input.lower() == suchwort.lower():
    print("Die Redewendung wurde gefunden!")
else:
    print("Die Redewendung wurde nicht gefunden.")

# Ausgabe: Die Redewendung wurde gefunden!
```

## Deep Dive
Ursprünglich in Programmiersprachen eingeführt, erlaubten Methoden wie `lower()` eine einfachere Textverarbeitung. Vor Unicode gab es ASCII, wo eine einfache Subtraktion ausreichte, um Großbuchstaben in Kleinbuchstaben umzuwandeln. Heutzutage gestaltet sich dies komplexer durch internationale Schriften und Regeln.

Alternativ könnten Reguläre Ausdrücke oder Schleifen zum Umwandeln genutzt werden, sind aber langsamer und weniger lesbar:

```python
import re
lower_text = re.sub(r'[A-Z]', lambda match: chr(ord(match.group(0))+32), text)
```

Die `lower()` Methode ist Teil des Python-String-Typs und arbeitet unter der Haube mit der Unicode-Datenbank, um korrekt zwischen Groß- und Kleinbuchstaben zu unterscheiden, selbst bei komplizierten Fällen wie dem deutschen `ß`, welches kein direktes Großschreibäquivalent hat.

## Siehe Auch
- Python Dokumentation zu Strings: https://docs.python.org/3/library/stdtypes.html#str.lower
- Unicode Standard: https://www.unicode.org/standard/standard.html
- Python `re` Modul Dokumentation: https://docs.python.org/3/library/re.html
