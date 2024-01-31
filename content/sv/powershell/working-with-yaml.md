---
title:                "Arbete med YAML"
date:                  2024-01-19
simple_title:         "Arbete med YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML (YAML Ain't Markup Language) används för konfigurationsfiler. Programmerare väljer YAML för sin läslighet och enkelhet att mappa komplexa datatyper.

## How to:
Installera `powershell-yaml` med:

```PowerShell
Install-Module -Name powershell-yaml
```

Läs en YAML-fil:

```PowerShell
# Förutsätter att "config.yaml" finns
$config = Get-Content -Path 'config.yaml' | ConvertFrom-Yaml
$config
```

Skriv en hash till en YAML-fil:

```PowerShell
$hash = @{
  path = 'C:\SomeFolder'
  settings = @{
    color = 'blue'
    size = 12
  }
}

$hash | ConvertTo-Yaml | Out-File -FilePath 'output.yaml'
# Kolla innehållet
Get-Content -Path 'output.yaml'
```

## Deep Dive
YAML introducerades 2001 som ett gränssnittvänligt alternativ till XML. Alternativ inkluderar JSON och TOML. Prestandan i PowerShell hanteras av `powershell-yaml`-modulen som använder .NET för parsing och generation av YAML.

## See Also
- YAML-specifikation: https://yaml.org/spec/
- GitHub `powershell-yaml`-modul: https://github.com/cloudbase/powershell-yaml
- Lära dig mer om YAML: https://learnxinyminutes.com/docs/yaml/
