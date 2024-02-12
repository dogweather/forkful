---
title:                "Робота з YAML"
aliases:
- /uk/powershell/working-with-yaml.md
date:                  2024-02-03T19:26:29.835805-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
YAML, або YAML Ain't Markup Language, — це мова серіалізації даних, зрозуміла для людини. Програмісти часто використовують її для файлів конфігурації та передачі даних між мовами. Її простота та читабельність роблять її особливо популярною для завдань, пов'язаних із налаштуванням середовищ, додатків або служб, де конфігурації є критично важливими і повинні бути легко зрозумілі та редаговані.

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
