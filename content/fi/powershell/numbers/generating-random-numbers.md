---
date: 2024-01-27 20:34:56.943011-07:00
description: "Satunnaislukujen tuottaminen PowerShelliss\xE4 tarkoittaa ennalta-arvaamattomien\
  \ numeeristen arvojen luontia m\xE4\xE4ritellyll\xE4 v\xE4lill\xE4. Ohjelmoijat\
  \ k\xE4ytt\xE4v\xE4t\u2026"
lastmod: '2024-03-13T22:44:56.776257-06:00'
model: gpt-4-0125-preview
summary: "Satunnaislukujen tuottaminen PowerShelliss\xE4 tarkoittaa ennalta-arvaamattomien\
  \ numeeristen arvojen luontia m\xE4\xE4ritellyll\xE4 v\xE4lill\xE4. Ohjelmoijat\
  \ k\xE4ytt\xE4v\xE4t\u2026"
title: Satunnaislukujen generointi
weight: 12
---

## Mikä ja miksi?
Satunnaislukujen tuottaminen PowerShellissä tarkoittaa ennalta-arvaamattomien numeeristen arvojen luontia määritellyllä välillä. Ohjelmoijat käyttävät tätä ominaisuutta monista syistä, mukaan lukien testaus, simulointi ja turvallisuustarkoitukset, joissa ennalta-arvaamattomuus tai todellisen maailman satunnaisuuden jäljitteleminen on elintärkeää.

## Kuinka:
PowerShell tarjoaa suoraviivaisen lähestymistavan satunnaislukujen tuottamiseen käyttämällä `Get-Random`-cmdlet-komentoa. Tämä cmdlet voi tuottaa satunnaislukuja joko oletusalueella tai määritellyllä välillä.

```PowerShell
# Tuota satunnaisluku väliltä 0 ja Int32.MaxValue
$randomNumber = Get-Random
Write-Output $randomNumber
```

Jos haluat määrittää alueen, käytä `-Minimum` ja `-Maximum` parametreja:

```PowerShell
# Tuota satunnaisluku väliltä 1 ja 100
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

Lisäkontrollin saavuttamiseksi voit luoda ilmentymän `System.Random`-luokasta:

```PowerShell
# Käyttäen System.Randomia numerosarjan tuottamiseen
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

Jos tarvitset satunnaista valintaa taulukosta tai kokoelmasta, `Get-Random` voi suoraan valita kohteen:

```PowerShell
# Satunnainen valinta taulukosta
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## Syväsukellus
`Get-Random`-cmdlet PowerShellissä hyödyntää .NET-luokkaa `System.Random` pseudosatunnaislukujen tuottamiseen. Nämä ovat "pseudo", koska ne käyttävät algoritmeja numerosekvenssien tuottamiseen, jotka vain vaikuttavat satunnaisilta. Useimmissa sovelluksissa tämä satunnaisuuden taso on riittävä. Kuitenkin käyttötarkoituksissa, jotka vaativat kryptografista turvallisuutta, `System.Random` ei ole sopiva sen ennustettavuuden vuoksi.

PowerShell ja .NET tarjoavat `System.Security.Cryptography.RNGCryptoServiceProvider`-luokan kryptografiseen satunnaisuuteen, joka soveltuu paremmin salausavainten tuottamiseen tai muihin turvallisuusherkkiin toimenpiteisiin:

```PowerShell
# Kryptografisesti turvalliset satunnaisluvut
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

Vaikka `Get-Random` ja `System.Random` tyydyttävät laajan tarpeiden kirjon satunnaisuudessa skriptauksessa ja sovelluslogiikassa, on olennaista valita oikea työkalu tehtävään, erityisesti turvallisuuskeskeisissä sovelluksissa, joissa ennustettavuus voi muodostaa haavoittuvuuden.
