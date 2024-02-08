---
title:                "Робота з JSON"
date:                  2024-02-03T19:23:52.071128-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Навіщо?

Інтеграція PowerShell з JSON (JavaScript Object Notation) зводиться до парсингу (читання) та генерації (запису) даних JSON, який є поширеним форматом для обміну даними в мережі. Програмісти працюють з JSON для взаємодії з веб-API, файлами конфігурації, або для спрощення обміну даними між різними мовами та платформами з огляду на його легковагість і незалежність від мови.

## Як це зробити:

### Парсинг JSON

Для читання або парсингу JSON у PowerShell, ви можете використовувати cmdlet `ConvertFrom-Json`. Вказавши рядок JSON, цей cmdlet перетворює його на об'єкт PowerShell.

```powershell
$json = '{"name": "John Doe", "age": 30, "city": "New York"}'
$person = $json | ConvertFrom-Json
$person.name
```

Зразок виводу:

```
John Doe
```

Цей приклад демонструє, як парсити простий рядок JSON для доступу до властивостей результуючого об'єкта.

### Генерація JSON

Для генерації JSON з об'єкта PowerShell ви можете використовувати cmdlet `ConvertTo-Json`. Це зручно для підготовки даних, які будуть відправлені на веб-сервіс або збережені у файл конфігурації.

```powershell
$person = [PSCustomObject]@{
    name = "Jane Doe"
    age = 25
    city = "Los Angeles"
}
$json = $person | ConvertTo-Json
Write-Output $json
```

Зразок виводу:

```json
{
    "name":  "Jane Doe",
    "age":  25,
    "city":  "Los Angeles"
}
```

Цей фрагмент коду створює об'єкт PowerShell, а потім перетворює його на рядок JSON.
