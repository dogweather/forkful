---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:21.834026-07:00
description: "Hvordan: Pythons innebygde `json`-bibliotek forenkler prosessen med\
  \ koding (konvertering av Python-objekter til JSON) og dekoding (konvertering av\
  \ JSON\u2026"
lastmod: '2024-03-13T22:44:40.382327-06:00'
model: gpt-4-0125-preview
summary: Pythons innebygde `json`-bibliotek forenkler prosessen med koding (konvertering
  av Python-objekter til JSON) og dekoding (konvertering av JSON til Python-objekter).
title: Arbeider med JSON
weight: 38
---

## Hvordan:
Pythons innebygde `json`-bibliotek forenkler prosessen med koding (konvertering av Python-objekter til JSON) og dekoding (konvertering av JSON til Python-objekter). Slik kan du bruke det:

### Koding av Python-objekter til JSON:
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

json_streng = json.dumps(data, indent=4)
print(json_streng)
```

**Utdata:**

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

### Dekoding av JSON til Python-objekter:
```python
json_streng = '''
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

data = json.loads(json_streng)
print(data)
```

**Utdata:**

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

### Arbeid med tredjepartsbiblioteker:
For kompleks JSON-håndtering, som skjemavalidering eller parsing av JSON-filer direkte fra URL-er, kan biblioteker som `requests` for HTTP-forespørsler og `jsonschema` for validering være nyttige.

#### Eksempel med `requests` for å parse JSON fra en URL:
```python
import requests

response = requests.get('https://api.example.com/data')
data = response.json()

print(data)
```

Dette kodeutdraget henter JSON-data fra en angitt URL og konverterer den direkte til et Python-objekt.

#### Bruk av `jsonschema` for å validere JSON:
Først, installer biblioteket via pip:

```bash
pip install jsonschema
```

Deretter, bruk det på følgende måte:

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

# Antatt at `data` er en ordbok oppnådd fra JSON-dekoding
prøv:
    validate(instance=data, schema=schema)
    print("Gyldige JSON-data.")
except jsonschema.exceptions.ValidationError as err:
    print("Valideringsfeil:", err)
```

Dette eksemplet validerer din Python-ordbok (opprettet fra dekodede JSON-data) mot et forhåndsdefinert skjema, for å sikre at dataene overholder forventede formater og typer.
