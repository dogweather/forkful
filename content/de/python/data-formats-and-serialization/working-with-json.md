---
title:                "Arbeiten mit JSON"
aliases:
- /de/python/working-with-json.md
date:                  2024-02-03T19:23:58.322411-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Die Arbeit mit JSON (JavaScript Object Notation) beinhaltet das Parsen von JSON-formatierten Strings in Python-Objekte und umgekehrt. Dies ist entscheidend für die Entwicklung von Webseiten und APIs, da JSON die Lingua Franca für den Datenaustausch zwischen Servern und Clients ist.

## Wie:

Die eingebaute `json`-Bibliothek in Python vereinfacht den Prozess der Kodierung (Konvertierung von Python-Objekten zu JSON) und Dekodierung (Konvertierung von JSON in Python-Objekte). So können Sie sie verwenden:

### Kodieren von Python-Objekten zu JSON:

```python
import json

data = {
    "name": "John Doe",
    "age": 30,
    "isEmployee": True,
    "addresses": [
        {"city": "New York", "zipCode": "10001"},
        {"city": "San Francisco", "zipCode": "94016"}
    ]
}

json_string = json.dumps(data, indent=4)
print(json_string)
```

**Ausgabe:**

```json
{
    "name": "John Doe",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "New York",
            "zipCode": "10001"
        },
        {
            "city": "San Francisco",
            "zipCode": "94016"
        }
    ]
}
```

### Dekodieren von JSON zu Python-Objekten:

```python
json_string = '''
{
    "name": "John Doe",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "New York",
            "zipCode": "10001"
        },
        {
            "city": "San Francisco",
            "zipCode": "94016"
        }
    ]
}
'''

data = json.loads(json_string)
print(data)
```

**Ausgabe:**

```python
{
    'name': 'John Doe', 
    'age': 30, 
    'isEmployee': True, 
    'addresses': [
        {'city': 'New York', 'zipCode': '10001'}, 
        {'city': 'San Francisco', 'zipCode': '94016'}
    ]
}
```

### Arbeiten mit Bibliotheken von Drittanbietern:

Für komplexere JSON-Verarbeitungen, wie z.B. Schema-Validierung oder das Parsen von JSON-Dateien direkt von URLs, können Bibliotheken wie `requests` für HTTP-Anfragen und `jsonschema` für Validierungen hilfreich sein.

#### Beispiel mit `requests` zur Analyse von JSON von einer URL:

```python
import requests

response = requests.get('https://api.example.com/data')
data = response.json()

print(data)
```

Dieser Schnipsel holt JSON-Daten von einer gegebenen URL und konvertiert sie direkt in ein Python-Objekt.

#### Verwendung von `jsonschema` zur Validierung von JSON:

Zuerst installieren Sie die Bibliothek über pip:

```bash
pip install jsonschema
```

Anschließend nutzen Sie sie wie folgt:

```python
from jsonschema import validate
import jsonschema

schema = {
    "type": "object",
    "properties": {
        "name": {"type": "string"},
        "age": {"type": "number"},
        "isEmployee": {"type": "boolean"},
    },
    "required": ["name", "age", "isEmployee"]
}

# Angenommen, `data` ist ein Wörterbuch, das durch JSON-Dekodierung erhalten wurde
try:
    validate(instance=data, schema=schema)
    print("Valid JSON data.")
except jsonschema.exceptions.ValidationError as err:
    print("Validierungsfehler:", err)
```

Dieses Beispiel validiert Ihr Python-Wörterbuch (erhalten aus decodierten JSON-Daten) gegen ein vordefiniertes Schema und stellt sicher, dass die Daten den erwarteten Formaten und Typen entsprechen.
