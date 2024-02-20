---
date: 2024-01-20 17:56:52.640255-07:00
description: "PowerShell-skripteiss\xE4 komentoriviparametrien lukeminen mahdollistaa\
  \ argumenttien vastaanottamisen suoraan k\xE4ytt\xE4j\xE4lt\xE4. T\xE4t\xE4 tehd\xE4\
  \xE4n sovelluksen\u2026"
lastmod: 2024-02-19 22:05:15.693761
model: gpt-4-1106-preview
summary: "PowerShell-skripteiss\xE4 komentoriviparametrien lukeminen mahdollistaa\
  \ argumenttien vastaanottamisen suoraan k\xE4ytt\xE4j\xE4lt\xE4. T\xE4t\xE4 tehd\xE4\
  \xE4n sovelluksen\u2026"
title: Komennoriviparametrien lukeminen
---

{{< edit_this_page >}}

## What & Why? / Mikä & Miksi?
PowerShell-skripteissä komentoriviparametrien lukeminen mahdollistaa argumenttien vastaanottamisen suoraan käyttäjältä. Tätä tehdään sovelluksen mukauttamiseksi eri tilanteisiin ilman skriptin muokkaamista.

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
