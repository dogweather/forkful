---
changelog:
- 2024-04-04 - dogweather - edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:34.962078-07:00
description: 'Wie man: #.'
lastmod: '2024-04-04T00:26:53.357557-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "Einen String gro\xDFschreiben"
weight: 2
---

## Wie man:

### Mit Pythons eingebauter Methode:
Python verfügt über eine eingebaute Methode `.capitalize()` für Strings, um diese Aufgabe einfach zu bewerkstelligen.

```python
my_string = "hallo welt"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**Ausgabe:**
```
Hallo welt
```

Hier ist meine eigene angepasste `capitalize()`-Funktion, die ich verwende, um diese Seite zu erstellen. Ich musste sicherstellen, dass spezielle Wörter wie **HTML** immer in Großbuchstaben bleiben. Dies demonstriert auch [Doctests](https://docs.python.org/3/library/doctest.html):

```python
def capitalize(string: str) -> str:
    """
    Macht den ersten Buchstaben eines Strings groß.
    Behandelt Sonderfälle wie "HTML".

    >>> capitalize("dies ist html, csv, xml und http (kein REPL).")
    'Dies ist HTML, CSV, XML und HTTP (kein REPL).'

    >>> capitalize("dies ist json, VBA, eine IDE und yaml in der CLI.")
    'Dies ist JSON, VBA, eine IDE und YAML in der CLI.'
    """
    return (
        string
            .capitalize()
            .replace('cli',  'CLI')
            .replace('csv',  'CSV')
            .replace('html', 'HTML')
            .replace('http', 'HTTP')
            .replace('ide',  'IDE')
            .replace('json', 'JSON')
            .replace('repl', 'REPL')
            .replace('vba',  'VBA')
            .replace('xml',  'XML')
            .replace('yaml', 'YAML')
    )

```

### Umgang mit mehreren Wörtern:
Für Szenarien, in denen jedes Wort in einem String mit einem Großbuchstaben beginnen soll (wie z.B. Titel), kann die Methode `.title()` angewendet werden.

```python
my_title = "python programmier grundlagen"
title_case = my_title.title()
print(title_case)
```
**Ausgabe:**
```
Python Programmier Grundlagen
```

### Verwendung von Drittanbieter-Bibliotheken:
Während Pythons Standardbibliothek für die grundlegende Großschreibung von Strings ausgestattet ist, können Bibliotheken wie `textblob` eine feinere Kontrolle bieten, insbesondere für die Verarbeitung natürlicher Sprache.

Stellen Sie zunächst sicher, dass `textblob` installiert ist:
```bash
pip install textblob
```

Verwenden Sie es dann, um Strings zu groß zu schreiben, wobei zu beachten ist, dass die Großschreibung von `textblob` je nach Verwendungskontext unterschiedlich funktionieren kann:

```python
from textblob import TextBlob

my_sentence = "dies ist ein testsatz"
blob = TextBlob(my_sentence)
capitalized_blob = TextBlob(blob.string.capitalize())
print(capitalized_blob)
```
**Ausgabe:**
```
Dies ist ein Testsatz
```

Denken Sie daran, während die Methoden `capitalize()` und `title()` universell nützlich sind, kann die Nutzung von Bibliotheken wie `textblob` zusätzliche Flexibilität für spezifische Anwendungen bieten.
