---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:16.233957-07:00
description: "YAML, tai YAML Ain't Markup Language, on ihmisen luettavissa oleva datan\
  \ serialisointikieli. Ohjelmoijat k\xE4ytt\xE4v\xE4t sit\xE4 usein konfiguraatiotiedostojen\
  \ ja\u2026"
lastmod: '2024-03-13T22:44:56.802800-06:00'
model: gpt-4-0125-preview
summary: YAML, tai YAML Ain't Markup Language, on ihmisen luettavissa oleva datan
  serialisointikieli.
title: "Ty\xF6skentely YAML:n kanssa"
weight: 41
---

## Mikä ja miksi?
YAML, tai YAML Ain't Markup Language, on ihmisen luettavissa oleva datan serialisointikieli. Ohjelmoijat käyttävät sitä usein konfiguraatiotiedostojen ja datan siirron eri kielten välillä. Sen yksinkertaisuus ja luettavuus tekevät siitä erityisen suositun tehtäviin, jotka liittyvät ympäristöjen, sovellusten tai palvelujen perustamiseen, joissa konfiguraatioiden on oltava kriittisiä ja niiden tulee olla helposti ymmärrettäviä ja muokattavia.

## Kuinka:
PowerShell ei oletuksena sisällä sisäänrakennettua cmdlet-komentoa YAML:n jäsentämiseen, mutta se toimii saumattomasti YAML:n kanssa, kun hyödynnät `powershell-yaml` -moduulia tai muunnat YAML:n PowerShell-objektiksi käyttäen `ConvertFrom-Json`-komentoa yhdessä työkalun, kuten `yq`, kanssa.

### Käyttäen `powershell-yaml`-moduulia:
Asenna ensin moduuli:
```PowerShell
Install-Module -Name powershell-yaml
```

YAML-tiedoston lukeminen:
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

PowerShell-objektin kirjoittaminen YAML-tiedostoon:
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

Esimerkki `output.yml`-tiedostosta:
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### YAML:n jäsentäminen `yq` ja `ConvertFrom-Json` avulla:
Toinen lähestymistapa sisältää `yq` käytön, joka on kevyt ja kannettava komentorivin YAML-prosessori. `yq` voi muuntaa YAML:n JSON:ksi, jonka PowerShell voi natiivisti jäsentää.

Varmista ensin, että `yq` on asennettu järjestelmääsi.
Suorita sitten:
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

Tämä menetelmä on erityisen hyödyllinen käyttäjille, jotka työskentelevät alustojen välisissä ympäristöissä tai suosivat JSON:n käyttöä PowerShellissä.
