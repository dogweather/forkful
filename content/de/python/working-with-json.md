---
title:                "Arbeiten mit JSON"
html_title:           "Python: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

JSON (JavaScript Object Notation) ist ein weit verbreitetes Format für den Austausch von Daten zwischen Anwendungen. Es ist leicht lesbar für Menschen und einfach zu parsen für Computer, wodurch es zu einer beliebten Wahl für die Übertragung von Daten wird.

## Wie man damit arbeitet

Um JSON in Python zu nutzen, müssen wir zuerst das `json` Modul importieren:

```Python
import json
```

Als nächstes können wir Daten von einer JSON-Datei lesen und in ein Python Dictionary konvertieren:

```Python
with open('daten.json') as f:
  daten = json.load(f)
```

Wir können nun auf die Daten in unserem `daten` Dictionary zugreifen, indem wir den entsprechenden Schlüssel verwenden. Zum Beispiel, wenn unsere JSON-Datei die Daten für einen Benutzer enthält, können wir auf den Benutzernamen wie folgt zugreifen:

```Python
benutzername = daten['benutzername']
```

Wir können auch ein Dictionary zu JSON konvertieren und in eine Datei schreiben:

```Python
neue_daten = {'alter': 25, 'ort': 'Berlin'}

with open('neue_daten.json', 'w') as f:
  json.dump(neue_daten, f)
```

## Tiefergehende Informationen

Der `json` Modul bietet verschiedene Funktionen, um mit JSON-Daten zu arbeiten. Zum Beispiel können wir Daten in einem lesbareren Format ausgeben, indem wir den Parameter `indent` verwenden:

```Python
json.dumps(daten, indent=2)
```

Wir können auch angeben, welche Art von Daten mit dem Parameter `sort_keys` sortiert werden sollen:

```Python
json.dumps(daten, sort_keys=True)
```

Darüber hinaus können wir auch eigene Funktionen erstellen, um Daten in JSON umzuwandeln, indem wir die `JSONEncoder` Klasse erweitern.

## Siehe auch

- [Offizielle Dokumentation für das json Modul](https://docs.python.org/3/library/json.html)
- [Ein Tutorial für die Arbeit mit JSON in Python](https://realpython.com/python-json/)
- [Ein Artikel mit Beispielen für die Verwendung des json Moduls](https://www.geeksforgeeks.org/working-with-json-data-in-python/).