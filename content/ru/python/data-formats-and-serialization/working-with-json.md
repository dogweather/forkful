---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:32.042215-07:00
description: "\u041A\u0430\u043A \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u0442\u044C: \u0414\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441\
  \ JSON \u0432 Python \u043D\u0435\u043E\u0431\u0445\u043E\u0434\u0438\u043C \u043C\
  \u043E\u0434\u0443\u043B\u044C `json`. \u0412\u043E\u0442 \u043A\u0440\u0430\u0442\
  \u043A\u043E\u0435 \u0440\u0443\u043A\u043E\u0432\u043E\u0434\u0441\u0442\u0432\u043E\
  : #."
lastmod: '2024-03-13T22:44:44.311202-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441 JSON \u0432\
  \ Python \u043D\u0435\u043E\u0431\u0445\u043E\u0434\u0438\u043C \u043C\u043E\u0434\
  \u0443\u043B\u044C `json`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

## Как использовать:
Для работы с JSON в Python необходим модуль `json`. Вот краткое руководство:

### Разбор JSON (`json.loads`):
```Python
import json

# Представим, что вы получили JSON от API
json_string = '{"name": "Alice", "age": 30, "city": "Wonderland"}'

# Преобразуйте JSON-строку в словарь Python
person = json.loads(json_string)

print(person)
```

### Пример вывода:
```Python
{'name': 'Alice', 'age': 30, 'city': 'Wonderland'}
```

### Генерация JSON (`json.dumps`):
```Python
import json

# Словарь Python
person_dict = {'name': 'Alice', 'age': 30, 'city': 'Wonderland'}

# Преобразуйте словарь в строку в формате JSON
person_json = json.dumps(person_dict)

print(person_json)
```

### Генерация JSON (`json.dumps`):
```Python
import json

# Словарь Python
person_dict = {'name': 'Alice', 'age': 30, 'city': 'Wonderland'}

# Преобразуйте словарь в строку в формате JSON
person_json = json.dumps(person_dict)

print(person_json)
```

## Глубокое погружение
JSON был предложен Дугласом Крокфордом в начале 2000-х как часть языка JavaScript, но быстро нашел применение в других языках благодаря своему легковесному формату. Альтернативы JSON включают XML и YAML, но JSON выигрывает за счет минимализма и скорости. Непосредственно в Python, JSON сериализуется в строки и десериализуется в словари или списки, что облегчает его программную обработку. Обратите внимание, что, хотя JSON похож на словарь Python, они не одинаковы — вы не можете использовать объекты и типы, специфичные для Python, в JSON.

## Смотрите также
- Официальный сайт JSON: [json.org](https://www.json.org)
- Документация модуля JSON в Python: [Python JSON](https://docs.python.org/3/library/json.html)
- Сравнение между JSON и XML: [JSON против XML](https://www.w3schools.com/js/js_json_xml.asp)
- Документация Python 3.x: [python.org](https://www.python.org/doc/)
