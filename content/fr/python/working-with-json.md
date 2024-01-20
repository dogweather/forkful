---
title:                "Manipulation de JSON"
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON, c'est du texte pour stocker et échanger des données. Les devs l'utilisent car il est léger, facile à lire et à décoder.

## How to:
### Charger un JSON
```python
import json

# Charger un JSON depuis une chaîne
json_string = '{"nom": "Dupont", "age": 30}'
data = json.loads(json_string)
print(data)
```
Sortie: `{'nom': 'Dupont', 'age': 30}`

### Convertir en JSON
```python
import json

# Données Python en JSON
data = {'nom': 'Dupont', 'age': 30}
json_string = json.dumps(data)
print(json_string)
```
Sortie: `{"nom": "Dupont", "age": 30}`

## Deep Dive
Le JSON est né en 2001, conçu par Douglas Crockford. Une alternative est XML, plus lourd. JSON s'implémente en Python avec le module `json`, inclus par défaut.

## See Also
- Documentation JSON Python : https://docs.python.org/3/library/json.html
- JSON vs XML : https://www.json.org/json-fr.html