---
title:                "Робота з JSON"
date:                  2024-01-19
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"

category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
JSON (JavaScript Object Notation) - це текстовий формат для зберігання та передачі даних. Програмісти використовують його через його легку читаність для людей і машин.

## How to: (Як робити:)
```PowerShell
# Читаємо JSON з файла
$json = Get-Content -Path 'example.json' | ConvertFrom-Json 

# Відображаємо об’єкт
$json

# Додавання нового ключа
$json.newKey = "newValue"

# Зберігаємо зміни у файл
$json | ConvertTo-Json | Set-Content -Path 'example.json'

# Вивід вмісту файла після змін
Get-Content -Path 'example.json'
```
```json
{
  "existingKey": "existingValue",
  "newKey": "newValue"
}
```

## Deep Dive (Поглиблено)
JSON започаткований з JavaScript, але зараз є мовно-незалежним форматом. Alternatives включають XML та YAML. Реалізація в PowerShell - надзвичайно проста з `ConvertFrom-Json` та `ConvertTo-Json`.

## See Also (Дивіться також)
- [JSON стандарт](https://www.json.org/json-en.html)
- [PowerShell Gallery модулі для JSON](https://www.powershellgallery.com/)
