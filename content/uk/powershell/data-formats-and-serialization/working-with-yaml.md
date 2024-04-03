---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:29.835805-07:00
description: "\u042F\u043A: PowerShell \u0437\u0430 \u0437\u0430\u043C\u043E\u0432\
  \u0447\u0443\u0432\u0430\u043D\u043D\u044F\u043C \u043D\u0435 \u043C\u0430\u0454\
  \ \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u043E\u0457 \u043A\u043E\u043C\
  \u0430\u043D\u0434\u0438 \u0434\u043B\u044F \u043F\u0430\u0440\u0441\u0438\u043D\
  \u0433\u0443 YAML, \u0430\u043B\u0435 \u0432\u0456\u043D \u043F\u0440\u0430\u0446\
  \u044E\u0454 \u0431\u0435\u0437\u043F\u0435\u0440\u0435\u0431\u0456\u0439\u043D\u043E\
  \ \u0437 YAML, \u043A\u043E\u043B\u0438 \u0432\u0438 \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u0454\u0442\u0435 \u043C\u043E\u0434\u0443\u043B\
  \u044C\u2026"
lastmod: '2024-03-13T22:44:49.684536-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u0437\u0430 \u0437\u0430\u043C\u043E\u0432\u0447\u0443\u0432\
  \u0430\u043D\u043D\u044F\u043C \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\
  \u0434\u043E\u0432\u0430\u043D\u043E\u0457 \u043A\u043E\u043C\u0430\u043D\u0434\u0438\
  \ \u0434\u043B\u044F \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443 YAML, \u0430\
  \u043B\u0435 \u0432\u0456\u043D \u043F\u0440\u0430\u0446\u044E\u0454 \u0431\u0435\
  \u0437\u043F\u0435\u0440\u0435\u0431\u0456\u0439\u043D\u043E \u0437 YAML, \u043A\
  \u043E\u043B\u0438 \u0432\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u043E\u0432\u0443\u0454\u0442\u0435 \u043C\u043E\u0434\u0443\u043B\u044C `powershell-yaml`\
  \ \u0430\u0431\u043E \u043F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u044E\u0454\
  \u0442\u0435 YAML \u0443 \u043E\u0431'\u0454\u043A\u0442 PowerShell \u0437\u0430\
  \ \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E `ConvertFrom-Json` \u0443\
  \ \u043F\u043E\u0454\u0434\u043D\u0430\u043D\u043D\u0456 \u0437 \u0456\u043D\u0441\
  \u0442\u0440\u0443\u043C\u0435\u043D\u0442\u043E\u043C \u043D\u0430 \u043A\u0448\
  \u0442\u0430\u043B\u0442 `yq`."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
weight: 41
---

## Як:
PowerShell за замовчуванням не має вбудованої команди для парсингу YAML, але він працює безперебійно з YAML, коли ви використовуєте модуль `powershell-yaml` або перетворюєте YAML у об'єкт PowerShell за допомогою `ConvertFrom-Json` у поєднанні з інструментом на кшталт `yq`.

### Використання модуля `powershell-yaml`:
Спочатку встановіть модуль:
```PowerShell
Install-Module -Name powershell-yaml
```

Для читання файлу YAML:
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

Для запису об'єкта PowerShell у файл YAML:
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

Приклад `output.yml`:
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### Парсинг YAML за допомогою `yq` та `ConvertFrom-Json`:
Інший підхід полягає у використанні `yq`, легковажного та портативного командного рядка обробника YAML. `yq` може конвертувати YAML у JSON, який PowerShell може рідно парсити.

Спочатку переконайтесь, що на вашій системі встановлено `yq`.
Потім запустіть:
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

Цей метод особливо корисний для користувачів, які працюють у крос-платформних середовищах або віддають перевагу використанню JSON у PowerShell.
