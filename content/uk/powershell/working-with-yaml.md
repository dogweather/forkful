---
title:                "Робота з YAML"
date:                  2024-01-19
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"

category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що і Чому?
YAML — це формат для зберігання та передачі даних, який легко читається людьми. Програмісти використовують YAML для конфігурацій файлів, інфраструктури як коду (IaC), та серіалізації даних.

## Як це зробити:
```PowerShell
# Інсталяція модуля для роботи з YAML
Install-Module -Name powershell-yaml

# Читання YAML файла
$yamlContent = Get-Content -Path 'config.yaml' -Raw
$config = ConvertFrom-Yaml -Yaml $yamlContent
Write-Host "Config name: " $config.name

# Створення YAML файла
$object = @{
    path = 'C:\example'
    timeout = 30
    featureFlags = @('FEATURE_A', 'FEATURE_B')
}
$yaml = ConvertTo-Yaml -Data $object
$yaml | Out-File -FilePath 'new-config.yaml'
Write-Host "YAML file created."
```

## Занурення в тему:
YAML (YAML Ain't Markup Language) започаткований уранніх2000-х ізпять буворієнтованими на зручність читання та підтримку комплексних датасетів. Альтернативи як JSON чи XML існують, але YAML виграє завдяки своїй читабельності. У Powershell робота з YAML потребує зовнішніх модулів, як `powershell-yaml`.

## Дивіться також:
- [Official YAML website](https://yaml.org)
- [PowerShell Gallery powershell-yaml Module](https://www.powershellgallery.com/packages/powershell-yaml)
- [GitHub Repository for powershell-yaml](https://github.com/cloudbase/powershell-yaml)
