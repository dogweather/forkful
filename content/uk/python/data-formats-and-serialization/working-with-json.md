---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:31.640250-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON (JavaScript Object\
  \ Notation) \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u0430\u043D\u0430\u043B\u0456\
  \u0437 \u0440\u044F\u0434\u043A\u0456\u0432 \u0443 \u0444\u043E\u0440\u043C\u0430\
  \u0442\u0456 JSON \u043D\u0430 \u043E\u0431'\u0454\u043A\u0442\u0438 Python \u0456\
  \ \u043D\u0430\u0432\u043F\u0430\u043A\u0438. \u0426\u0435 \u0432\u0430\u0436\u043B\
  \u0438\u0432\u043E \u0434\u043B\u044F \u0440\u043E\u0437\u0440\u043E\u0431\u043A\
  \u0438 \u0432\u0435\u0431-\u0441\u0430\u0439\u0442\u0456\u0432 \u0442\u0430 API,\u2026"
lastmod: '2024-03-13T22:44:48.618118-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON (JavaScript Object Notation)\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u0430\u043D\u0430\u043B\u0456\u0437\
  \ \u0440\u044F\u0434\u043A\u0456\u0432 \u0443 \u0444\u043E\u0440\u043C\u0430\u0442\
  \u0456 JSON \u043D\u0430 \u043E\u0431'\u0454\u043A\u0442\u0438 Python \u0456 \u043D\
  \u0430\u0432\u043F\u0430\u043A\u0438. \u0426\u0435 \u0432\u0430\u0436\u043B\u0438\
  \u0432\u043E \u0434\u043B\u044F \u0440\u043E\u0437\u0440\u043E\u0431\u043A\u0438\
  \ \u0432\u0435\u0431-\u0441\u0430\u0439\u0442\u0456\u0432 \u0442\u0430 API,\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
weight: 38
---

## Що і чому?

Робота з JSON (JavaScript Object Notation) включає аналіз рядків у форматі JSON на об'єкти Python і навпаки. Це важливо для розробки веб-сайтів та API, оскільки JSON є лінгва франка для обміну даними між серверами та клієнтами.

## Як:

Вбудована бібліотека `json` Python спрощує процес кодування (перетворення об'єктів Python на JSON) та декодування (перетворення JSON на об'єкти Python). Ось як ви можете її використати:

### Кодування об'єктів Python у JSON:

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

**Вивід:**

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

### Декодування JSON у об'єкти Python:

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

**Вивід:**

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

### Робота з бібліотеками сторонніх виробників:

Для складнішої обробки JSON, такої як валідація схеми чи аналіз файлів JSON безпосередньо з URL, корисними можуть бути бібліотеки, як-от `requests` для HTTP-запитів та `jsonschema` для валідації.

#### Приклад з `requests` для аналізу JSON з URL:

```python
import requests

response = requests.get('https://api.example.com/data')
data = response.json()

print(data)
```

Цей фрагмент отримує дані JSON з заданого URL і безпосередньо перетворює їх в об'єкт Python.

#### Використання `jsonschema` для валідації JSON:

Спочатку встановіть бібліотеку через pip:

```bash
pip install jsonschema
```

Потім використовуйте її так:

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

# Передбачаючи, що `data` це словник, отриманий з декодування JSON
try:
    validate(instance=data, schema=schema)
    print("Дані у форматі JSON валідні.")
except jsonschema.exceptions.ValidationError as err:
    print("Помилка валідації:", err)
```

Цей приклад валідує ваш словник Python (отриманий з декодованих даних JSON) проти попередньо визначеної схеми, переконуючись, що дані відповідають очікуваним форматам і типам.
