---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:52.071128-07:00
description: "\u0406\u043D\u0442\u0435\u0433\u0440\u0430\u0446\u0456\u044F PowerShell\
  \ \u0437 JSON (JavaScript Object Notation) \u0437\u0432\u043E\u0434\u0438\u0442\u044C\
  \u0441\u044F \u0434\u043E \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443 (\u0447\
  \u0438\u0442\u0430\u043D\u043D\u044F) \u0442\u0430 \u0433\u0435\u043D\u0435\u0440\
  \u0430\u0446\u0456\u0457 (\u0437\u0430\u043F\u0438\u0441\u0443) \u0434\u0430\u043D\
  \u0438\u0445 JSON, \u044F\u043A\u0438\u0439 \u0454 \u043F\u043E\u0448\u0438\u0440\
  \u0435\u043D\u0438\u043C \u0444\u043E\u0440\u043C\u0430\u0442\u043E\u043C \u0434\
  \u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:49.686123-06:00'
model: gpt-4-0125-preview
summary: "\u0406\u043D\u0442\u0435\u0433\u0440\u0430\u0446\u0456\u044F PowerShell\
  \ \u0437 JSON (JavaScript Object Notation) \u0437\u0432\u043E\u0434\u0438\u0442\u044C\
  \u0441\u044F \u0434\u043E \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443 (\u0447\
  \u0438\u0442\u0430\u043D\u043D\u044F) \u0442\u0430 \u0433\u0435\u043D\u0435\u0440\
  \u0430\u0446\u0456\u0457 (\u0437\u0430\u043F\u0438\u0441\u0443) \u0434\u0430\u043D\
  \u0438\u0445 JSON, \u044F\u043A\u0438\u0439 \u0454 \u043F\u043E\u0448\u0438\u0440\
  \u0435\u043D\u0438\u043C \u0444\u043E\u0440\u043C\u0430\u0442\u043E\u043C \u0434\
  \u043B\u044F \u043E\u0431\u043C\u0456\u043D\u0443 \u0434\u0430\u043D\u0438\u043C\
  \u0438 \u0432 \u043C\u0435\u0440\u0435\u0436\u0456."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
weight: 38
---

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
