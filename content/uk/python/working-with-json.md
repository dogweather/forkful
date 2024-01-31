---
title:                "Робота з JSON"
date:                  2024-01-19
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/working-with-json.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
JSON - це формат обміну даними. Програмісти використовують JSON для зберігання і передачі структурованої інформації між сервером та клієнтом або між різними програмами.

## Як робити:
```Python
import json

# Серіалізація: Python до JSON
data_py = {'name': 'Іван', 'age': 30, 'city': 'Київ'}
data_json = json.dumps(data_py)
print(data_json) # Виведе: {"name": "Іван", "age": 30, "city": "Київ"}

# Десеріалізація: JSON до Python
data_py2 = json.loads(data_json)
print(data_py2) # Виведе: {'name': 'Іван', 'age': 30, 'city': 'Київ'}
```

## Поглиблені знання:
JSON (JavaScript Object Notation) з'явився з JavaScript, але став мово-незалежним. Альтернативою є XML, але JSON простіший і швидший. В Python робота з JSON реалізована завдяки модулю `json`, який легко серіалізує (з Python у JSON) та десеріалізує (з JSON у Python) дані.

## Дивіться також:
- [Офіційна документація Python для модуля json](https://docs.python.org/3/library/json.html)
- [W3Schools JSON tutorial](https://www.w3schools.com/js/js_json_intro.asp)
- [JSON на MDN Web Docs](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON)
