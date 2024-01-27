---
title:                "Arbeid med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON står for JavaScript Object Notation og lar oss lagre og utveksle data kompakt og strukturert. Programmerere bruker JSON fordi det er lettleselig for mennesker og lett å tolke for maskiner, noe som gjør det til et ideelt format for datautveksling mellom servere og webapplikasjoner.

## How to:
```PowerShell
# Leser en JSON-fil
$json = Get-Content -Path 'C:\data\example.json' | ConvertFrom-Json

# Viser JSON-dataene
$json

# Endrer en verdi og lagrer tilbake til filen
$json.name = 'Ola Nordmann'
$json | ConvertTo-Json | Set-Content -Path 'C:\data\example_modified.json'
```
Output:
```
name          age
----          ---
Ola Nordmann  30
```

```PowerShell
# Oppretter et PSObject og konverterer det til JSON
$person = [PSCustomObject]@{
    name = 'Kari Nordmann'
    age = 28
}

# Konverterer til JSON og skriver til konsollen
$person | ConvertTo-Json
```
Output:
```
{
    "name":  "Kari Nordmann",
    "age":  28
}
```

## Deep Dive
JSON ble introdusert i 2001 og er basert på JavaScripts objektlitteraler, men er språkuavhengig og brukes i mange programmeringsspråk. Alternativer til JSON inkluderer XML og YAML, men JSON er ofte foretrukket for dens enkelhet. Når du arbeider med JSON i PowerShell, håndterer cmdlet'er som `ConvertTo-Json` og `ConvertFrom-Json` konverteringer og sikrer at datastrukturer blir bibeholdt.

## See Also
- [Understanding JSON](https://www.json.org/json-en.html)
- [PowerShell's ConvertFrom-Json documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/convertfrom-json?view=powershell-7)
