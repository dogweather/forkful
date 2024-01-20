---
title:                "Arbeiten mit json"
html_title:           "Python: Arbeiten mit json"
simple_title:         "Arbeiten mit json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/working-with-json.md"
---

{{< edit_this_page >}}

## Was ist JSON und warum verwenden Programmierer es?

JSON steht für "JavaScript Object Notation" und ist ein kompaktes Datenaustauschformat, das häufig von Programmierern verwendet wird. Es ermöglicht das Lesen, Schreiben und Verarbeiten von Daten in einem einfachen und leicht lesbaren Format. Dies erleichtert die Kommunikation zwischen verschiedenen Systemen und Programmiersprachen.

## Wie funktioniert es?

Um JSON in Python zu verwenden, muss zunächst das integrierte Modul `json` importiert werden. Dann kann ein JSON-Objekt mit Hilfe von `json.loads()` in ein Python-Dictionary oder mit `json.load()` in ein JSON-Dateiobjekt umgewandelt werden. Umgekehrt kann ein Python-Dictionary mit `json.dumps()` in ein JSON-Objekt und mit `json.dump()` in eine Datei geschrieben werden.

```
# Beispiel 1: JSON zu Python

import json

# JSON-String
json_string = '{"name": "Max", "age": 25, "city": "Berlin"}'

# JSON in Python-Dictionary umwandeln
python_dict = json.loads(json_string)

print(python_dict)
# Output: {'name': 'Max', 'age': 25, 'city': 'Berlin'}
```

```
# Beispiel 2: Python zu JSON

import json

# Python-Dictionary
python_dict = {'name': 'Max', 'age': 25, 'city': 'Berlin'}

# Python-Dictionary in JSON umwandeln
json_string = json.dumps(python_dict)

print(json_string)
# Output: {"name": "Max", "age": 25, "city": "Berlin"}
```

## Tiefergehende Informationen

JSON wurde entwickelt, um eine Alternative zum häufig verwendeten XML-Datenaustauschformat zu bieten. Im Vergleich zu XML ist JSON kompakter, leichter zu lesen und zu schreiben und wird von vielen modernen Programmiersprachen nativ unterstützt.

Eine Alternative zu JSON ist das CSV-Format, welches jedoch aufgrund seiner strukturierten Datenrepräsentation nicht so flexibel wie JSON ist.

Bei der Implementierung von JSON sollten immer Sicherheitsvorkehrungen getroffen werden, da unvalidierte Daten möglicherweise bösartigen Code enthalten könnten.

## Weitere Informationen

Für weitere Informationen zu JSON und dessen Verwendung in Python, folgen Sie den nachstehenden Links:

- [Offizielle JSON Dokumentation von Python](https://docs.python.org/3/library/json.html)
- [Einführung in JSON für Python-Anfänger](https://realpython.com/python-json/)