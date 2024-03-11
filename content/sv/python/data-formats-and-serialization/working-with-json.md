---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:09.047747-07:00
description: "Att arbeta med JSON (JavaScript Object Notation) inneb\xE4r att tolka\
  \ JSON-formaterade str\xE4ngar till Python-objekt och tv\xE4rtom. Detta \xE4r avg\xF6\
  rande f\xF6r webb-\u2026"
lastmod: '2024-03-11T00:14:10.817432-06:00'
model: gpt-4-0125-preview
summary: "Att arbeta med JSON (JavaScript Object Notation) inneb\xE4r att tolka JSON-formaterade\
  \ str\xE4ngar till Python-objekt och tv\xE4rtom. Detta \xE4r avg\xF6rande f\xF6\
  r webb-\u2026"
title: Arbeta med JSON
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med JSON (JavaScript Object Notation) innebär att tolka JSON-formaterade strängar till Python-objekt och tvärtom. Detta är avgörande för webb- och API-utveckling eftersom JSON är det gemensamma språket för utbyte av data mellan servrar och klienter.

## Hur man gör:

Pythons inbyggda `json`-bibliotek förenklar processen att koda (konvertera Python-objekt till JSON) och avkoda (konvertera JSON till Python-objekt). Så här kan du använda det:

### Koda Python-objekt till JSON:

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

### Avkoda JSON till Python-objekt:

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

### Arbeta med tredjepartsbibliotek:

För komplex hantering av JSON, såsom schemavalidation eller tolkning av JSON-filer direkt från URL:er, kan bibliotek som `requests` för HTTP-förfrågningar och `jsonschema` för validation vara till hjälp.

#### Exempel med `requests` för att tolka JSON från en URL:

```python
import requests

response = requests.get('https://api.example.com/data')
data = response.json()

print(data)
```

Detta kodsnutt hämtar JSON-data från en given URL och konverterar det direkt till ett Python-objekt.

#### Använda `jsonschema` för att validera JSON:

Först, installera biblioteket via pip:

```bash
pip install jsonschema
```

Använd det sedan så här:

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

# Antag att `data` är en ordbok som erhållits från JSON-avkodning
försök:
    validate(instance=data, schema=schema)
    print("Giltiga JSON-data.")
except jsonschema.exceptions.ValidationError as err:
    print("Valideringsfel:", err)
```

Detta exempel validerar din Python-ordbok (erhållen från avkodade JSON-data) mot ett fördefinierat schema, vilket säkerställer att datan överensstämmer med förväntade format och typer.
