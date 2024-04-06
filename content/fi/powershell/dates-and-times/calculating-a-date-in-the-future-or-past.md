---
date: 2024-01-20 17:31:34.019672-07:00
description: "How to: PowerShellissa p\xE4iv\xE4m\xE4\xE4r\xE4laskenta onnistuu helposti\
  \ `Get-Date`-komennon avulla, joka on osa j\xE4rjestelm\xE4n automaattista toimintaa.\
  \ Ennen\u2026"
lastmod: '2024-04-05T22:51:10.946343-06:00'
model: gpt-4-1106-preview
summary: "PowerShellissa p\xE4iv\xE4m\xE4\xE4r\xE4laskenta onnistuu helposti `Get-Date`-komennon\
  \ avulla, joka on osa j\xE4rjestelm\xE4n automaattista toimintaa."
title: "Tulevan tai menneen p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen"
weight: 26
---

## How to:
Laske päivä 10 päivän päähän:

```PowerShell
$Tulevaisuus = (Get-Date).AddDays(10)
$Tulevaisuus
```

Laske päivä 10 päivää menneisyyteen:

```PowerShell
$Menneisyys = (Get-Date).AddDays(-10)
$Menneisyys
```

## Deep Dive
PowerShellissa päivämäärälaskenta onnistuu helposti `Get-Date`-komennon avulla, joka on osa järjestelmän automaattista toimintaa. Ennen PowerShellia ja .NET Frameworkia tämä olisi vaatinut monimutkaisempia laskutoimituksia ja funktioita.

Vaihtoehtoja:
- Käytä `.NET`-luokkaa `[DateTime]` suoraan.
- Vanhemmissa skriptikielissä, kuten batch skripteissä, laskenta on ollut hankalampaa.

Yksityiskohdat:
- `Get-Date` tuottaa nykyisen ajan ja päivämäärän.
- `AddDays()` metodi hyväksyy myönteisiä arvoja tulevaisuuden päivämäärien laskentaan ja kielteisiä arvoja menneisyyden päivämäärien laskentaan.

## See Also
- [PowerShell-dokumentaatio Get-Date -komennosta](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-date)
- [.NET DateTime -dokumentaatio](https://docs.microsoft.com/dotnet/api/system.datetime)
