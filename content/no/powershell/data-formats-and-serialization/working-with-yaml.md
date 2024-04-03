---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:17.310866-07:00
description: "YAML, eller YAML Ain't Markup Language, er et menneskelesbart serialiseringsspr\xE5\
  k for data. Programmerere bruker det ofte for konfigurasjonsfiler og\u2026"
lastmod: '2024-03-13T22:44:41.037272-06:00'
model: gpt-4-0125-preview
summary: "YAML, eller YAML Ain't Markup Language, er et menneskelesbart serialiseringsspr\xE5\
  k for data."
title: Arbeider med YAML
weight: 41
---

## Hva og hvorfor?
YAML, eller YAML Ain't Markup Language, er et menneskelesbart serialiseringsspråk for data. Programmerere bruker det ofte for konfigurasjonsfiler og dataoverføring mellom språk. Dets enkelhet og lesbarhet gjør det spesielt populært for oppgaver som involverer oppsett av miljøer, applikasjoner eller tjenester der konfigurasjoner er avgjørende og bør være lett å forstå og redigere.

## Hvordan:
PowerShell kommer som standard ikke med en innebygd cmdlet for å tolke YAML, men det fungerer sømløst med YAML når du bruker `powershell-yaml`-modulen eller konverterer YAML til et PowerShell-objekt ved hjelp av `ConvertFrom-Json` i kombinasjon med et verktøy som `yq`.

### Bruke `powershell-yaml`-modulen:
Først, installer modulen:
```PowerShell
Install-Module -Name powershell-yaml
```

For å lese en YAML-fil:
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

For å skrive et PowerShell-objekt til en YAML-fil:
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

Eksempel på `output.yml`:
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### Tolke YAML med `yq` og `ConvertFrom-Json`:
En annen tilnærming innebærer å bruke `yq`, en lett og bærbar kommandolinje-YAML-prosessor. `yq` kan konvertere YAML til JSON, som PowerShell kan tolke naturlig.

Først, sørg for at `yq` er installert på systemet ditt.
Deretter kjør:
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

Denne metoden er spesielt nyttig for brukere som arbeider i plattformuavhengige miljøer eller foretrekker å bruke JSON i PowerShell.
