---
title:                "Praca z JSON"
date:                  2024-01-19
simple_title:         "Praca z JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

JSON, czyli JavaScript Object Notation, to łatwy do odczytania format wymiany danych. Programiści używają go do komunikacji między serwerami i aplikacjami webowymi oraz do zapisu konfiguracji.

## Jak to zrobić:

```PowerShell
# Zapisywanie obiektu do JSON
$user = @{
    Name     = 'Jan'
    Surname  = 'Nowak'
    Email    = 'jan.nowak@poczta.pl'
}
$user | ConvertTo-Json | Set-Content -Path 'user.json'

# Wczytywanie JSON z pliku
$content = Get-Content -Path 'user.json' | ConvertFrom-Json
Write-Output $content.Name  # Wypisze 'Jan'
```

## Deep Dive

JSON w świecie IT pojawił się w latach 2000 i szybko stał się popularny jako prostsza alternatywa dla XML. PowerShell natywnie obsługuje JSON poprzez cmdlety `ConvertTo-Json` i `ConvertFrom-Json`. Chociaż są inne formacje jak YAML czy TOML, JSON pozostaje liderem ze względu na uniwersalność i szerokie wsparcie w różnych językach programowania.

## Zobacz także:

- Oficjalna strona JSON: [json.org](https://www.json.org/json-pl.html)
- Dokumentacja PowerShell dla `ConvertTo-Json`: [Microsoft Docs ConvertTo-Json](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/convertto-json)
- Dokumentacja PowerShell dla `ConvertFrom-Json`: [Microsoft Docs ConvertFrom-Json](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/convertfrom-json)
