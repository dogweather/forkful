---
title:                "Arbeid med json"
html_title:           "PowerShell: Arbeid med json"
simple_title:         "Arbeid med json"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## Hva og hvorfor? 

Arbeidet med JSON innebærer å behandle data i et filformat som er bygget på åpen standard. Dette formatet er ofte brukt for å lagre og utveksle data mellom applikasjoner og systemer. Programmerere bruker JSON for å lagre og behandle data på en effektiv måte.

## Hvordan: 

```PowerShell
# Konvertere JSON til PowerShell-objekt
$jsonObject = ConvertFrom-Json -InputObject $json

# Opprette en ny JSON-fil 
$jsonObject | ConvertTo-Json | Out-File -FilePath "C:\bruker\data.json"

# Hente data fra en JSON-fil og behandle den 
$jsonData = Get-Content -Path "C:\bruker\data.json" | ConvertFrom-Json 
foreach ($item in $jsonData) {
    # Gjøre noe med hvert element
    $item.Name 
}
```

## Dypdykk: 

JSON ble utviklet i 2001 som et alternativ til XML for å lagre og utveksle data. Mange programmeringsspråk har innebygd støtte for JSON, inkludert PowerShell. Alternativer til JSON inkluderer XML og CSV-filer, men JSON anses ofte som en mer effektiv og fleksibel løsning for å håndtere større mengder data. JSON står for "JavaScript Object Notation" og brukes ofte i webutvikling for å sende og motta data mellom klient og server.

## Se også: 

Offisiell Microsoft-dokumentasjon for [ConvertTo-Json](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/convertto-json?view=powershell-7) og [ConvertFrom-Json](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/convertfrom-json?view=powershell-7) cmdletene.

En grundig forklaring av JSON og hvordan du arbeider med det i PowerShell finner du på [PowerShellMagazine.com](https://www.powershellmagazine.com/2021/01/21/working-with-json-in-powershell/).