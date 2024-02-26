---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:11.790279-07:00
description: "Ty\xF6skentely JSON-muotoisten merkkijonojen (JavaScript Object Notation)\
  \ kanssa sis\xE4lt\xE4\xE4 JSON-muotoisten merkkijonojen j\xE4sent\xE4misen Python-objekteiksi\
  \ ja\u2026"
lastmod: '2024-02-25T18:49:53.138920-07:00'
model: gpt-4-0125-preview
summary: "Ty\xF6skentely JSON-muotoisten merkkijonojen (JavaScript Object Notation)\
  \ kanssa sis\xE4lt\xE4\xE4 JSON-muotoisten merkkijonojen j\xE4sent\xE4misen Python-objekteiksi\
  \ ja\u2026"
title: "Ty\xF6skentely JSON:n kanssa"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Työskentely JSON-muotoisten merkkijonojen (JavaScript Object Notation) kanssa sisältää JSON-muotoisten merkkijonojen jäsentämisen Python-objekteiksi ja päinvastoin. Tämä on ratkaisevan tärkeää web- ja API-kehityksessä, koska JSON toimii yleiskielenä palvelinten ja asiakkaiden välisen datan vaihdossa.

## Kuinka:

Pythonin sisäänrakennettu `json`-kirjasto yksinkertaistaa enkoodausprosessia (Python-objektien muuntaminen JSON-muotoon) ja dekoodausta (JSONin muuntaminen Python-objekteiksi). Näin voit käyttää sitä:

### Python-objektien enkoodaus JSON-muotoon:

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

**Tuloste:**

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

### JSONin dekoodaus Python-objekteiksi:

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

**Tuloste:**

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

### Kolmansien osapuolien kirjastojen käyttö:

Monimutkaisemmissa JSON-käsittelyissä, kuten skeemavalidaatiossa tai JSON-tiedostojen jäsentämisessä suoraan URL-osoitteista, voi olla hyödyllistä käyttää kirjastoja kuten `requests` HTTP-pyyntöihin ja `jsonschema` validaatioon.

#### Esimerkki `requests`-kirjaston kanssa JSONin jäsentämiseen URL:sta:

```python
import requests

response = requests.get('https://api.example.com/data')
data = response.json()

print(data)
```

Tämä koodinpätkä noutaa JSON-dataa annetusta URL-osoitteesta ja muuntaa sen suoraan Python-objektiksi.

#### `jsonschema`-kirjaston käyttö JSONin validaatioon:

Asenna ensin kirjasto pip:n kautta:

```bash
pip install jsonschema
```

Käytä sitten seuraavasti:

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

# Olettaen, että `data` on sanakirja, joka on saatu JSON-dekoodauksesta
try:
    validate(instance=data, schema=schema)
    print("Validi JSON-data.")
except jsonschema.exceptions.ValidationError as err:
    print("Validaatiovirhe:", err)
```

Tässä esimerkissä validoidaan Python-sanakirjasi (joka on saatu JSON-datan dekoodauksesta) ennalta määritettyä skeemaa vasten, varmistetaan, että data vastaa odotettuja formaatteja ja tyyppejä.
