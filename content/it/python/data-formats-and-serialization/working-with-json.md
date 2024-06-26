---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:09.115011-07:00
description: "Come fare: La libreria integrata `json` di Python semplifica il processo\
  \ di codifica (conversione di oggetti Python in JSON) e decodifica (conversione\
  \ di\u2026"
lastmod: '2024-03-13T22:44:43.022383-06:00'
model: gpt-4-0125-preview
summary: La libreria integrata `json` di Python semplifica il processo di codifica
  (conversione di oggetti Python in JSON) e decodifica (conversione di JSON in oggetti
  Python).
title: Lavorare con JSON
weight: 38
---

## Come fare:
La libreria integrata `json` di Python semplifica il processo di codifica (conversione di oggetti Python in JSON) e decodifica (conversione di JSON in oggetti Python). Ecco come puoi utilizzarla:

### Codifica di oggetti Python in JSON:
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

**Output:**

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

### Decodifica di JSON in oggetti Python:
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

**Output:**

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

### Lavorare con librerie di terze parti:
Per la gestione complessa di JSON, come la validazione dello schema o l'analisi di file JSON direttamente da URL, possono essere utili librerie come `requests` per le richieste HTTP e `jsonschema` per la validazione.

#### Esempio con `requests` per analizzare JSON da un URL:
```python
import requests

response = requests.get('https://api.example.com/data')
data = response.json()

print(data)
```

Questo frammento recupera dati JSON da un dato URL e li converte direttamente in un oggetto Python.

#### Utilizzare `jsonschema` per validare JSON:
Prima, installare la libreria tramite pip:

```bash
pip install jsonschema
```

Poi, utilizzarla come segue:

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

# Assumendo che `data` sia un dizionario ottenuto dalla decodifica di JSON
try:
    validate(instance=data, schema=schema)
    print("Dati JSON validi.")
except jsonschema.exceptions.ValidationError as err:
    print("Errore di validazione:", err)
```

Questo esempio valida il tuo dizionario Python (ottenuto dai dati JSON decodificati) contro uno schema predefinito, assicurando che i dati si conformino ai formati e ai tipi attesi.
