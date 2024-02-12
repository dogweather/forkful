---
title:                "Работа с JSON"
aliases: - /ru/powershell/working-with-json.md
date:                  2024-01-29T00:04:03.370673-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/powershell/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
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
