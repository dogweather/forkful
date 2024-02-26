---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:11.705240-07:00
description: "Praca z JSON (JavaScript Object Notation) obejmuje parsowanie ci\u0105\
  g\xF3w sformatowanych w JSON na obiekty Pythona i odwrotnie. Jest to kluczowe dla\
  \ rozwoju\u2026"
lastmod: '2024-02-25T18:49:33.397469-07:00'
model: gpt-4-0125-preview
summary: "Praca z JSON (JavaScript Object Notation) obejmuje parsowanie ci\u0105g\xF3\
  w sformatowanych w JSON na obiekty Pythona i odwrotnie. Jest to kluczowe dla rozwoju\u2026"
title: Praca z JSON
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z JSON (JavaScript Object Notation) obejmuje parsowanie ciągów sformatowanych w JSON na obiekty Pythona i odwrotnie. Jest to kluczowe dla rozwoju stron internetowych i API, ponieważ JSON jest lingua franca do wymiany danych pomiędzy serwerami i klientami.

## Jak to zrobić:

Wbudowana biblioteka `json` w Pythonie upraszcza proces kodowania (konwertowania obiektów Pythona na JSON) i dekodowania (konwertowania JSON na obiekty Pythona). Oto jak możesz jej użyć:

### Kodowanie obiektów Pythona na JSON:

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

**Wynik:**

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

### Dekodowanie JSON na obiekty Pythona:

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

**Wynik:**

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

### Praca z bibliotekami stron trzecich:

Dla złożonej obsługi JSON, takiej jak walidacja schematu czy parsowanie plików JSON bezpośrednio z URL-i, pomocne mogą być biblioteki takie jak `requests` do zapytań HTTP i `jsonschema` do walidacji.

#### Przykład z użyciem `requests` do parsowania JSON z URL:

```python
import requests

response = requests.get('https://api.example.com/data')
data = response.json()

print(data)
```

Ten fragment kodu pobiera dane JSON z danego URL i bezpośrednio konwertuje je na obiekt Pythona.

#### Używanie `jsonschema` do walidacji JSON:

Najpierw zainstaluj bibliotekę za pomocą pip:

```bash
pip install jsonschema
```

Następnie użyj jej w ten sposób:

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

# Zakładając, że `data` to słownik uzyskany z dekodowania JSON
try:
    validate(instance=data, schema=schema)
    print("Dane JSON są poprawne.")
except jsonschema.exceptions.ValidationError as err:
    print("Błąd walidacji:", err)
```

Ten przykład weryfikuje twój słownik Pythona (uzyskany z dekodowanych danych JSON) względem predefiniowanego schematu, zapewniając, że dane odpowiadają oczekiwanym formatom i typom.
