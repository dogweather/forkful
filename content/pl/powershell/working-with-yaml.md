---
title:                "Praca z yaml"
date:                  2024-01-19
html_title:           "Arduino: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Praca z YAML to zarządzanie danymi w formacie, który jest prosty dla człowieka i maszyny. Programiści używają go, gdy potrzebują konfiguracji, serializacji danych lub komunikacji między usługami.

## How to: (Jak to zrobić:)
PowerShell umożliwia pracę z YAML za pomocą modułów, takich jak 'powershell-yaml'. Oto przykład wczytania i zapisu danych YAML.

```PowerShell
# Instalacja modułu PowerShell-Yaml
Install-Module -Name powershell-yaml

# Wczytanie zawartości YAML z pliku
$yamlContent = Get-Content -Path 'example.yaml' | Out-String
$yamlObject = ConvertFrom-Yaml -InputObject $yamlContent
$yamlObject

# Tworzenie nowego obiektu YAML
$newYamlObject = @{
    "hello" = "world"
    "count" = 42
    "items" = @("item1", "item2")
}

# Zapis obiektu do pliku YAML
$newYamlObject | ConvertTo-Yaml | Set-Content -Path 'newExample.yaml'
```

## Deep Dive (Głębsze spojrzenie)
YAML, czyli "YAML Ain't Markup Language", powstał w 2001 roku jako alternatywa dla XML i JSON. Charakteryzuje się czytelnością i składnią opartą o wcięcia. W PowerShellu trzeba użyć zewnętrznych modułów, bo nie ma wbudowanego wsparcia dla YAML. PowerShell-yaml to popularny wybór, ale są też inne jak PYYaml czy YamlDotNet.

## See Also (Zobacz także)
- Oficjalna strona YAML: [https://yaml.org](https://yaml.org)
- Repozytorium modułu PowerShell-Yaml na GitHubie: [https://github.com/cloudbase/powershell-yaml](https://github.com/cloudbase/powershell-yaml)
- Dokumentacja PowerShell: [https://docs.microsoft.com/en-us/powershell/](https://docs.microsoft.com/en-us/powershell/)
