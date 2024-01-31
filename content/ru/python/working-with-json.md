---
title:                "Работа с JSON"
date:                  2024-01-29T00:05:32.042215-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/python/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
JSON (JavaScript Object Notation) является повсеместно используемым форматом обмена данными в Интернете. Программисты используют JSON для удобной передачи данных между серверами и веб-клиентами благодаря его простоте и тому, что он изначально понятен JavaScript, а следовательно, и веб-браузерам.

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

### Пример вывода:
```Python
'{"name": "Alice", "age": 30, "city": "Wonderland"}'
```

## Глубокое погружение
JSON был предложен Дугласом Крокфордом в начале 2000-х как часть языка JavaScript, но быстро нашел применение в других языках благодаря своему легковесному формату. Альтернативы JSON включают XML и YAML, но JSON выигрывает за счет минимализма и скорости. Непосредственно в Python, JSON сериализуется в строки и десериализуется в словари или списки, что облегчает его программную обработку. Обратите внимание, что, хотя JSON похож на словарь Python, они не одинаковы — вы не можете использовать объекты и типы, специфичные для Python, в JSON.

## Смотрите также
- Официальный сайт JSON: [json.org](https://www.json.org)
- Документация модуля JSON в Python: [Python JSON](https://docs.python.org/3/library/json.html)
- Сравнение между JSON и XML: [JSON против XML](https://www.w3schools.com/js/js_json_xml.asp)
- Документация Python 3.x: [python.org](https://www.python.org/doc/)
