---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:15:53.264059-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"

category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä ja miksi?)
Nykyisen päivämäärän haku on ajan tietojen saamista järjestelmästä. Ohjelmoijat käyttävät sitä, kun tarvitaan aikaleimoja, päivämäärien validointia tai ajastettuja toimintoja.

## How to: (Kuinka tehdä:)
```PowerShell
# Hae nykyinen päivämäärä ja aika
$nyt = Get-Date
Write-Output $nyt

# Tulostaa esimerkiksi: sunnuntai 9. huhtikuuta 2023 14:56:35
```

```PowerShell
# Hae nykyinen päivämäärä ilman aikaa
$paivamaara = (Get-Date).Date
Write-Output $paivamaara

# Tulostaa esimerkiksi: 9.4.2023 0:00:00
```

```PowerShell
# Muotoile päivämäärä haluamallasi tavalla
$muotoiltu = Get-Date -Format "dd.MM.yyyy"
Write-Output $muotoiltu

# Tulostaa esimerkiksi: 09.04.2023
```

## Deep Dive (Syväsukellus)
PowerShellin `Get-Date` cmdlet on ollut käytössä alusta alkaen, ja se on yleinen tapa hakea koneen paikallinen aika. Vaihtoehtoisia tapoja sisältävät .NET-luokat, kuten `[System.DateTime]`, mutta `Get-Date` on yksinkertaisempi PowerShellissa. Haku perustuu käyttöjärjestelmän kelloon, ja tulokseen vaikuttavat käyttäjän alueasetukset. Cmdlet mahdollistaa päivämäärän muotoilun ja parsimisen, jolloin pystyt luomaan juuri tarvitsemasi esitystavan aikatiedoille.

## See Also (Katso myös)
- Microsoftin dokumentaatio `Get-Date`-cmdletistä: [PowerShell Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.2)
- Tietoa .NET:n `System.DateTime` luokasta: [DateTime Struct](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- Lisää muotoilumerkkijonoja päivämäärien esitystapaan: [Standard DateTime Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)
