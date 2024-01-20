---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was und Warum?

Das Extrahieren von Teilstrings ist das Herausnehmen bestimmter Teile eines Strings. In der Programmierung wird dies getan, um spezifische Daten, wie Benutzer-IDs, Tags und Schlüsselwörter zu isolieren.

## Wie geht das:

In Python können wir substrings mit Indexierung oder der `find()` und `slice()` Methode extrahieren. Hier sind einige Beispiele:

```python
# Beispiel String
s = "Hallo, mein Name ist Python Programmierer!"

# Substring durch Indexierung
print(s[7:11])         # Ausgabe: "mein"

# Substring durch find() und slice()
start = s.find("Name")
end = s.find(" ist")
substring = s.slice(start, end)
print(substring)       # Ausgabe: "Name"
```

## Vertiefung

Substring-Extraktion ist nicht nur auf Python beschränkt, es ist eine grundlegende Funktion in fast allen Programmiersprachen. Es geht bis in die frühen Tage der Programmierung zurück, als Speicher eine kostbare Ressource war.

Es gibt viele Methoden, um Substrings in Python zu extrahieren, und einige davon verwenden Bibliotheken wie `re` (Regular Expressions) und `numpy`. Während Indexierung und `find()` & `slice()` in den meisten Fällen ausreichen, können diese Bibliotheken in komplexeren Szenarien nützlich sein.

Genauer gesagt, Python speichert Strings intern als Arrays von Bytes. Dies ermöglicht es dem Programm, auf bestimmte Zeichen zuzugreifen, indem es ihren Index verwendet.

## Siehe Auch

- Python Dokumentation für Strings: [Link](https://docs.python.org/3/library/stdtypes.html#string-methods)
- Python-Reguläre Ausdrücke: [Link](https://docs.python.org/3/library/re.html)
- Tutorial zum extrahieren von Substrings in Python: [Link](https://www.journaldev.com/23674/python-remove-character-from-string)