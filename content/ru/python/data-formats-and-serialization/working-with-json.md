---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:32.042215-07:00
description: "JSON (JavaScript Object Notation) \u044F\u0432\u043B\u044F\u0435\u0442\
  \u0441\u044F \u043F\u043E\u0432\u0441\u0435\u043C\u0435\u0441\u0442\u043D\u043E\
  \ \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C\u044B\u043C \u0444\
  \u043E\u0440\u043C\u0430\u0442\u043E\u043C \u043E\u0431\u043C\u0435\u043D\u0430\
  \ \u0434\u0430\u043D\u043D\u044B\u043C\u0438 \u0432 \u0418\u043D\u0442\u0435\u0440\
  \u043D\u0435\u0442\u0435. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\
  \u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442\
  \ JSON \u0434\u043B\u044F \u0443\u0434\u043E\u0431\u043D\u043E\u0439 \u043F\u0435\
  \u0440\u0435\u0434\u0430\u0447\u0438\u2026"
lastmod: '2024-02-25T18:49:42.064675-07:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \u044F\u0432\u043B\u044F\u0435\u0442\u0441\
  \u044F \u043F\u043E\u0432\u0441\u0435\u043C\u0435\u0441\u0442\u043D\u043E \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C\u044B\u043C \u0444\u043E\u0440\
  \u043C\u0430\u0442\u043E\u043C \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\
  \u043D\u043D\u044B\u043C\u0438 \u0432 \u0418\u043D\u0442\u0435\u0440\u043D\u0435\
  \u0442\u0435. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\
  \u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442 JSON \u0434\u043B\
  \u044F \u0443\u0434\u043E\u0431\u043D\u043E\u0439 \u043F\u0435\u0440\u0435\u0434\
  \u0430\u0447\u0438\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
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
