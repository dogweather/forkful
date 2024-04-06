---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:52.071128-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0414\u043B\u044F \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u0430\u0431\u043E\
  \ \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443 JSON \u0443 PowerShell, \u0432\
  \u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\u043A\u043E\u0440\u0438\
  \u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 cmdlet `ConvertFrom-Json`.\
  \ \u0412\u043A\u0430\u0437\u0430\u0432\u0448\u0438 \u0440\u044F\u0434\u043E\u043A\
  \ JSON, \u0446\u0435\u0439 cmdlet \u043F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\
  \u044E\u0454\u2026"
lastmod: '2024-03-13T22:44:49.686123-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u0430\u0431\
  \u043E \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443 JSON \u0443 PowerShell,\
  \ \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 cmdlet `ConvertFrom-Json`."
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
