---
title:                "YAML-tiedostojen käsittely"
date:                  2024-01-19
simple_title:         "YAML-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML on helppolukuinen datan serialisointimuoto, jota käytetään määrittelyihin ja konfiguraatioihin. Ohjelmoijat suosivat sitä selkeyden ja ihmiselle luettavan syntaksin vuoksi.

## How to:
PowerShellissa YAML-tiedostojen käsittelyyn tarvitaan usein lisäkirjasto, kuten `powershell-yaml`-moduuli. Asenna moduuli ja käytä sitä lukeaksesi ja kirjoittaaksesi YAML-tiedostoja.

```PowerShell
# Moduulin asennus
Install-Module -Name powershell-yaml

# YAML-tiedoston lukeminen
$yamlContent = Get-Content -Path 'config.yaml' | Out-String
$yamlObject = ConvertFrom-Yaml $yamlContent
$yamlObject

# YAML-tiedoston kirjoittaminen
$hashTable = @{
    path = 'kansio/polku'
    timeout = 30
}
$yamlOutput = ConvertTo-Yaml $hashTable
$yamlOutput | Set-Content -Path 'output.yaml'
```

## Deep Dive
YAML (YAML Ain't Markup Language) lanseerattiin 2001 helpottamaan datan esittämistä ja jakamista ohjelmistojen kesken. JSON ja XML ovat suosittuja vaihtoehtoja, mutta YAML erottuu luettavuudellaan. PowerShell käyttää .NET-kirjastoja YAML-datan käsittelyyn, joten PowerShell-moduulit kuten `powershell-yaml` hyödyntävät näitä kirjastoja YAML-tiedon parsimiseen ja muuntamiseen.

## See Also
- [YAML-aineisto GitHubissa](https://github.com/yaml/yaml)
- [PowerShell Gallery - powershell-yaml](https://www.powershellgallery.com/packages/powershell-yaml)
- [YAML-ohjeet ja spesifikaatiot](https://yaml.org/spec/1.2/spec.html)
