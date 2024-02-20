---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:03.370673-07:00
description: "JSON (JavaScript Object Notation \u2014 \u0437\u0430\u043F\u0438\u0441\
  \u044C \u043E\u0431\u044A\u0435\u043A\u0442\u043E\u0432 JavaScript) \u2014 \u044D\
  \u0442\u043E \u043B\u0435\u0433\u043A\u0438\u0439 \u0444\u043E\u0440\u043C\u0430\
  \u0442 \u0434\u0430\u043D\u043D\u044B\u0445, \u0443\u0434\u043E\u0431\u043D\u044B\
  \u0439 \u0434\u043B\u044F \u0447\u0442\u0435\u043D\u0438\u044F \u0438 \u0437\u0430\
  \u043F\u0438\u0441\u0438 \u043B\u044E\u0434\u044C\u043C\u0438, \u0430 \u0442\u0430\
  \u043A\u0436\u0435 \u0434\u043B\u044F \u0440\u0430\u0437\u0431\u043E\u0440\u0430\
  \ \u0438\u2026"
lastmod: 2024-02-19 22:05:04.464093
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation \u2014 \u0437\u0430\u043F\u0438\u0441\u044C\
  \ \u043E\u0431\u044A\u0435\u043A\u0442\u043E\u0432 JavaScript) \u2014 \u044D\u0442\
  \u043E \u043B\u0435\u0433\u043A\u0438\u0439 \u0444\u043E\u0440\u043C\u0430\u0442\
  \ \u0434\u0430\u043D\u043D\u044B\u0445, \u0443\u0434\u043E\u0431\u043D\u044B\u0439\
  \ \u0434\u043B\u044F \u0447\u0442\u0435\u043D\u0438\u044F \u0438 \u0437\u0430\u043F\
  \u0438\u0441\u0438 \u043B\u044E\u0434\u044C\u043C\u0438, \u0430 \u0442\u0430\u043A\
  \u0436\u0435 \u0434\u043B\u044F \u0440\u0430\u0437\u0431\u043E\u0440\u0430 \u0438\
  \u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
---

{{< edit_this_page >}}

## Что и почему?
JSON (JavaScript Object Notation — запись объектов JavaScript) — это легкий формат данных, удобный для чтения и записи людьми, а также для разбора и генерации машинами. Программисты используют JSON для обмена данными между веб-клиентами и серверами или для хранения данных, потому что он прост и стал веб-стандартом.

## Как это сделать:
### Чтение JSON
```PowerShell
# Предположим, 'data.json' содержит {"name": "John", "age": 30}
$json = Get-Content -Path 'data.json' | ConvertFrom-Json
# Вывод имени
$json.name  # Вывод: John
```

### Запись JSON
```PowerShell
$person = @{name='Jane'; age=25}
$person | ConvertTo-Json | Set-Content -Path 'person.json'
# person.json теперь содержит: 
# {
#     "age":  25,
#     "name":  "Jane"
# }
```

### Изменение JSON
```PowerShell
$json = Get-Content -Path 'person.json' | ConvertFrom-Json
$json.age = 26
$json | ConvertTo-Json | Set-Content -Path 'person.json'
# person.json теперь обновляет возраст Jane на 26
```

## Глубокое погружение
JSON стал основным средством для работы с данными в Интернете с начала 2000-х, забрав у XML пальму первенства из-за своей простоты. Альтернативы JSON включают YAML и более новый TOML, но JSON властвует благодаря своей широкой поддержке и совместимости с синтаксисом объектов JavaScript. При работе с JSON в PowerShell встроенные командлеты `ConvertFrom-Json` и `ConvertTo-Json` являются мощными инструментами, но следует обратить внимание на их пределы глубины и тип `[PSCustomObject]` PowerShell, используемый при преобразовании из JSON.

## Смотрите также
- [JSON.org](https://www.json.org/json-en.html) для синтаксиса и основ JSON
- [MDN Веб-документация по JSON](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON) для освещения аспектов JavaScript
