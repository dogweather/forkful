---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:20.837517-07:00
description: "YAML, czyli YAML Ain't Markup Language, to j\u0119zyk seryalizacji danych\
  \ czytelny dla cz\u0142owieka. Programi\u015Bci cz\u0119sto u\u017Cywaj\u0105 go\
  \ do plik\xF3w konfiguracyjnych i\u2026"
lastmod: '2024-03-13T22:44:35.650247-06:00'
model: gpt-4-0125-preview
summary: "YAML, czyli YAML Ain't Markup Language, to j\u0119zyk seryalizacji danych\
  \ czytelny dla cz\u0142owieka. Programi\u015Bci cz\u0119sto u\u017Cywaj\u0105 go\
  \ do plik\xF3w konfiguracyjnych i\u2026"
title: Praca z YAML
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
