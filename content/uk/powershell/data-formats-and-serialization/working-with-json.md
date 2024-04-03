---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:52.071128-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : #."
lastmod: '2024-03-13T22:44:49.686123-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
weight: 38
---

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
