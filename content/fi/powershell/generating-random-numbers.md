---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:49:57.687349-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Satunnaislukujen generointi tarkoittaa uusien, ennustamattomien numeroiden luomista. Ohjelmoijat käyttävät niitä pelimekaniikoissa, tietoturvassa ja simulaatioissa, tarjoten arvaamattomuutta ja monipuolisuutta.

## How to: (Kuinka tehdään:)
```PowerShell
# Arvonnan peruskäyttö
$random = Get-Random
Write-Output $random

# Määritellään lukujen arvonta-alue
$randomInRange = Get-Random -Minimum 10 -Maximum 30
Write-Output $randomInRange

# Satunnaisluku listasta
$list = 1..10
$randomFromList = Get-Random -InputObject $list
Write-Output $randomFromList

# Useamman satunnaisluvun generointi
$randomNumbers = 1..5 | ForEach-Object { Get-Random -Minimum 1 -Maximum 100 }
$randomNumbers -join ', '
```
Esimerkkitulosteet:
```
1764461834
17
9
65, 2, 43, 21, 78
```

## Deep Dive (Syväkatsaus):
Satunnaislukujen generointi on ollut keskeinen osa tietokoneohjelmia jo vuosikymmenten ajan. Historiallisesti, tietokoneet ovat käyttäneet erilaisia menetelmiä, kuten algoritmipohjaisia pseudosatunnaislukugeneraattoreita (PRNGs), joiden tulokset näyttävät satunnaisilta mutta ovat toistettavissa. 

PowerShellin `Get-Random`-komento käyttää .NET-luokkaa `[System.Random]`, joka on PRNG. Tämän generoidut luvut ovat riittävän satunnaisia useimpiin sovelluksiin, mutta eivät sovellu korkean turvallisuuden vaatimuksiin, kuten salausavainten luontiin. Vaihtoehtoina on olemassa entropialähteisiin perustuvia menetelmiä, jotka tarjoavat parempaa satunnaisuutta.

Toimintaa syvemmin ymmärtääkseen ohjelmoijan on syytä tietää, että satunnaisluvut eivät vakio-olosuhteissa ole "oikeasti" satunnaisia ja että niiden "satunnaisuus" voidaan paljastaa analysoimalla luvut. Tämän vuoksi kriittisiin sovelluksiin on suositeltavaa käyttää turvallisempia menetelmiä.

## See Also (Katso Myös):
- Microsoftin dokumentaatio `Get-Random` komennosta: [Tästä](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random)
- .NET `[System.Random]` luokasta: [Tästä](https://docs.microsoft.com/en-us/dotnet/api/system.random)
- Turvalliset satunnaislukugeneraattorit (CSPRNGs): [Tästä](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.randomnumbergenerator)