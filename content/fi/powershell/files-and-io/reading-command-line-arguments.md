---
date: 2024-01-20 17:56:52.640255-07:00
description: "How to / Kuinka: Aikoinaan, kun PowerShell syntyi, se korvasi vanhat\
  \ komentosarjaohjelmat, kuten CMD-komennot, ja toi mukanaan edistyneemm\xE4t ominaisuudet\u2026"
lastmod: '2024-04-05T21:53:58.373274-06:00'
model: gpt-4-1106-preview
summary: "Aikoinaan, kun PowerShell syntyi, se korvasi vanhat komentosarjaohjelmat,\
  \ kuten CMD-komennot, ja toi mukanaan edistyneemm\xE4t ominaisuudet argumenttien\
  \ k\xE4sittelyyn."
title: Komennoriviparametrien lukeminen
weight: 23
---

## How to / Kuinka:
```PowerShell
# Skriptin tallentaminen nimellä 'ExampleScript.ps1'

# Argumenttien käsittely
param(
  [string]$name = "World",
  [int]$number = 42
)

# Argumenttien käyttö
Write-Host "Hei $name! Sinun numerosi on $number."

# Komentoriviltä ajo
# .\ExampleScript.ps1 -name "Taneli" -number 7

# Tulostus:
# Hei Taneli! Sinun numerosi on 7.
```

## Deep Dive / Syväsukellus:
Aikoinaan, kun PowerShell syntyi, se korvasi vanhat komentosarjaohjelmat, kuten CMD-komennot, ja toi mukanaan edistyneemmät ominaisuudet argumenttien käsittelyyn. Sen $args-muuttuja on perinteinen tapa päästä käsiksi komentorivin argumentteihin, mutta param()-lohko tarjoaa paremman kontrollin ja syötteen validoinnin. 

PowerShellissä param()-lohkoa käytetään määrittelemään oletusarvot ja datatyypit argumenteille, mikä tekee komentosarjoista luotettavampia ja helpompia ymmärtää. Muita skriptikieliä käytettäessä argumentteja saatetaan lukea eri tavoin, mutta PowerShellin tapa on erityisen voimakas siihen sisältyvän objekti-mallinsa ja parametrien tyypitysten ansiosta.

## See Also / Katso Myös:
- Microsoftin virallinen dokumentaatio PowerShellin about_Functions ja about_Functions_Advanced_Parameters osioihin: [linkki](https://docs.microsoft.com/en-us/powershell/)
