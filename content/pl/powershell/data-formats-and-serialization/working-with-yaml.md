---
title:                "Praca z YAML"
aliases:
- /pl/powershell/working-with-yaml.md
date:                  2024-02-03T19:26:20.837517-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
YAML, czyli YAML Ain't Markup Language, to język seryalizacji danych czytelny dla człowieka. Programiści często używają go do plików konfiguracyjnych i transmisji danych między językami. Jego prostota i czytelność sprawiają, że jest szczególnie popularny do zadań związanych z konfiguracją środowisk, aplikacji lub usług, gdzie konfiguracje są kluczowe i powinny być łatwo zrozumiałe i edytowalne.

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
