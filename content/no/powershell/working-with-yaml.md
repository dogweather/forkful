---
title:                "Arbeid med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML er et format for data serialisering, enkelt for mennesker å lese og skrive. Programmerere bruker YAML for konfigurasjonsfiler og datautveksling, fordi det er lett å forstå og integrere i moderne utviklingsstacks.

## How to:
Installere `powershell-yaml` modulen:
```PowerShell
Install-Module -Name powershell-yaml
```

Laste YAML innhold i en PowerShell-variabel:
```PowerShell
Import-Module powershell-yaml
$configData = @"
name: Ola Nordmann
occupation: Programmer
languages:
  - Norwegian
  - English
"@
$yamlObject = ConvertFrom-Yaml $configData
$yamlObject
```

Output vil se slik ut:
```
name       : Ola Nordmann
occupation : Programmer
languages  : {Norwegian, English}
```

Lagre PowerShell objekt til en YAML-fil:
```PowerShell
$person = @{
  name = 'Kari Nordmann'
  occupation = 'Programmer'
  languages = @('Norwegian', 'English')
}
$person | ConvertTo-Yaml | Out-File -FilePath 'person.yaml'
```

## Deep Dive
YAML, som står for "YAML Ain't Markup Language", ble introdusert i starten av 2000-tallet som et enklere alternativ til XML. Json er et annet alternativ, men YAML er ofte foretrukket for sin leslighet. I PowerShell, krever arbeid med YAML en ekstern modul fordi det ikke er innebygget støtte, slik som med Json.

## See Also
- PowerShell-Yaml modul: https://github.com/cloudbase/powershell-yaml
- YAML offisielle hjemmeside: https://yaml.org
- Lære mer om YAML syntaks: https://learnxinyminutes.com/docs/yaml/
