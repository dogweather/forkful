---
title:                "JSON-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"

category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) on dataformaatti tietojen tallentamiseen ja välittämiseen. Ohjelmoijat käyttävät sitä, koska se on helppolukuinen ihmisille ja koneille ymmärrettävä.

## How to:
```PowerShell
# JSON-muotoisen tiedon luominen ja tallentaminen tiedostoon
$jsonObject = @{
    nimi = 'Matti'
    ikä = 30
    taidot = @('PowerShell', 'Azure')
}
$jsonData = $jsonObject | ConvertTo-Json
$jsonData | Out-File -FilePath 'tiedot.json'

# JSON-tiedoston lukeminen ja muuttamisen objektiksi
$jsonFromFile = Get-Content -Path 'tiedot.json' | ConvertFrom-Json
Write-Output $jsonFromFile.nimi  # Tulostaa 'Matti'
```

## Deep Dive:
PowerShell lisäsi JSON-tuen versiosta 3.0 lähtien, mikä helpotti web-palvelimien ja muiden ohjelmistojen kanssa kommunikoimista. JSON vaihtoehtoihin kuuluu XML, mutta JSON on yleensä kevyempi ja nopeampi. Implementaatiossa PowerShell käyttää .NET-kirjastoja, kuten Newtonsoft.Json-pakettia JSON-käsittelyyn.

## See Also:
- [JSON-spesifikaatio](https://www.json.org/json-en.html)
