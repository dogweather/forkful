---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:03.370673-07:00
description: ''
lastmod: '2024-04-05T21:59:57.246235-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

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
