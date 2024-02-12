---
title:                "Werken met JSON"
aliases:
- /nl/powershell/working-with-json.md
date:                  2024-01-28T22:10:30.305703-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/powershell/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
JSON (JavaScript Object Notation) is een lichtgewicht gegevensformaat dat gemakkelijk door mensen te lezen en te schrijven is, en eenvoudig door machines te parseren en te genereren. Programmeurs werken met JSON om gegevens uit te wisselen tussen webcliënten en -servers of om gegevens op te slaan omdat het eenvoudig is en een webstandaard is geworden.

## Hoe:
### JSON Lezen
```PowerShell
# Stel 'data.json' bevat {"name": "John", "age": 30}
$json = Get-Content -Path 'data.json' | ConvertFrom-Json
# Geef de naam weer
$json.name  # Geeft weer: John
```

### JSON Schrijven
```PowerShell
$person = @{name='Jane'; age=25}
$person | ConvertTo-Json | Set-Content -Path 'person.json'
# person.json bevat nu: 
# {
#     "age":  25,
#     "name":  "Jane"
# }
```

### JSON Wijzigen
```PowerShell
$json = Get-Content -Path 'person.json' | ConvertFrom-Json
$json.age = 26
$json | ConvertTo-Json | Set-Content -Path 'person.json'
# person.json heeft nu Jane's leeftijd bijgewerkt naar 26
```

## Diep Duiken
JSON is sinds de vroege jaren 2000 de go-to voor webgegevens, en nam de troon over van XML vanwege zijn eenvoud.
Alternatieven voor JSON zijn onder andere YAML en het nieuwere TOML, maar JSON heerst vanwege de wijdverspreide ondersteuning en de afstemming met de objectensyntax van JavaScript. Bij het werken met JSON in PowerShell zijn de ingebouwde `ConvertFrom-Json` en `ConvertTo-Json` cmdlets krachtig, maar let op hun dieptelimieten en het `[PSCustomObject]` PowerShell-type dat wordt gebruikt bij het omzetten vanuit JSON.

## Zie Ook
- [JSON.org](https://www.json.org/json-en.html) voor de syntax en basisprincipes van JSON
- [MDN Web Docs over JSON](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON) voor de JavaScript-kant
