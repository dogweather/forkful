---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:20.837517-07:00
description: "Jak: PowerShell domy\u015Blnie nie posiada wbudowanego polecenia cmdlet\
  \ do analizy YAML, ale dzia\u0142a bezproblemowo z YAML, gdy wykorzystuje si\u0119\
  \ modu\u0142\u2026"
lastmod: '2024-03-13T22:44:35.650247-06:00'
model: gpt-4-0125-preview
summary: "PowerShell domy\u015Blnie nie posiada wbudowanego polecenia cmdlet do analizy\
  \ YAML, ale dzia\u0142a bezproblemowo z YAML, gdy wykorzystuje si\u0119 modu\u0142\
  \ `powershell-yaml` lub konwertuje YAML na obiekt PowerShell za pomoc\u0105 `ConvertFrom-Json`\
  \ w po\u0142\u0105czeniu z narz\u0119dziem takim jak `yq`."
title: Praca z YAML
weight: 41
---

## Jak:
PowerShell domyślnie nie posiada wbudowanego polecenia cmdlet do analizy YAML, ale działa bezproblemowo z YAML, gdy wykorzystuje się moduł `powershell-yaml` lub konwertuje YAML na obiekt PowerShell za pomocą `ConvertFrom-Json` w połączeniu z narzędziem takim jak `yq`.

### Korzystając z modułu `powershell-yaml`:
Najpierw zainstaluj moduł:
```PowerShell
Install-Module -Name powershell-yaml
```

Aby odczytać plik YAML:
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

Aby zapisać obiekt PowerShell do pliku YAML:
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

Przykładowy `output.yml`:
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### Analiza YAML za pomocą `yq` i `ConvertFrom-Json`:
Inne podejście polega na użyciu `yq`, lekkiego i przenośnego procesora wiersza poleceń YAML. `yq` może konwertować YAML na JSON, który PowerShell może natywnie analizować.

Najpierw upewnij się, że `yq` jest zainstalowane na twoim systemie.
Następnie uruchom:
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

Ta metoda jest szczególnie użyteczna dla użytkowników, którzy pracują w środowiskach wieloplatformowych lub preferują używanie JSON w PowerShell.
