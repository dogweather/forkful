---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:02.058396-07:00
description: "Travailler avec JSON (JavaScript Object Notation) implique d'analyser\
  \ des cha\xEEnes format\xE9es JSON en objets Python et vice versa. Cela est crucial\
  \ pour le\u2026"
lastmod: '2024-03-13T22:44:57.259502-06:00'
model: gpt-4-0125-preview
summary: "Travailler avec JSON (JavaScript Object Notation) implique d'analyser des\
  \ cha\xEEnes format\xE9es JSON en objets Python et vice versa."
title: Travailler avec JSON
weight: 38
---

## Quoi et pourquoi ?

Travailler avec JSON (JavaScript Object Notation) implique d'analyser des chaînes formatées JSON en objets Python et vice versa. Cela est crucial pour le développement web et d’API, car JSON est la lingua franca pour l'échange de données entre serveurs et clients.

## Comment faire :

La bibliothèque intégrée `json` de Python simplifie le processus d'encodage (convertir des objets Python en JSON) et de décodage (convertir JSON en objets Python). Voici comment vous pouvez l'utiliser :

### Encoder des objets Python en JSON :

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

**Sortie :**

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

### Décoder du JSON en objets Python :

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

**Sortie :**

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

### Travailler avec des bibliothèques tierces :

Pour une gestion complexe du JSON, telles que la validation de schéma ou l'analyse de fichiers JSON directement à partir d'URLs, des bibliothèques comme `requests` pour les requêtes HTTP et `jsonschema` pour la validation peuvent être utiles.

#### Exemple avec `requests` pour analyser du JSON depuis une URL :

```python
import requests

response = requests.get('https://api.example.com/data')
data = response.json()

print(data)
```

Ce snippet récupère les données JSON depuis une URL donnée et les convertit directement en un objet Python.

#### Utiliser `jsonschema` pour valider le JSON :

D'abord, installez la bibliothèque via pip :

```bash
pip install jsonschema
```

Ensuite, utilisez-la comme suit :

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

# En supposant que `data` est un dictionnaire obtenu après décodage JSON
try:
    validate(instance=data, schema=schema)
    print("Données JSON valides.")
except jsonschema.exceptions.ValidationError as err:
    print("Erreur de validation :", err)
```

Cet exemple valide votre dictionnaire Python (obtenu à partir de données JSON décodées) par rapport à un schéma prédéfini, en s'assurant que les données sont conformes aux formats et types attendus.
