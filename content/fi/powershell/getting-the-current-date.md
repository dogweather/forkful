---
title:                "Tämänhetkisen päivämäärän saaminen"
html_title:           "PowerShell: Tämänhetkisen päivämäärän saaminen"
simple_title:         "Tämänhetkisen päivämäärän saaminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä ja miksi? 
Päivämäärän hankkiminen tarkoittaa nykyisen päivämäärän saamista. Ohjelmoijat tekevät tätä usein skriptien, raporttien tai sovellusten kehityksessä.

## Kuinka: 
```PowerShell
$nykyinenPäivämäärä = Get-Date
```

## Perusteet: 
Päivämäärän hankkiminen on erittäin tärkeä osa ohjelmointia, sillä se auttaa pitämään tietokannat ja tiedot ajan tasalla. Se myös mahdollistaa ajastetun tehtävien suorittamisen ja raporttien luomisen tarkasteltavaksi.

PowerShellissä on valmis funktio, joka on nimeltään Get-Date ja se palauttaa nykyisen päivämäärän. Tämän jälkeen päivämäärä voidaan tallentaa muuttujaan ja käyttää sitä haluttuun tarkoitukseen. 

## Syventävää tietoa: 
Päivämäärän hankkimiseen on olemassa myös muita tapoja kuin Get-Date funktio. Voit esimerkiksi käyttää CMDlet date tai käyttää .NET-ohjelmointikielen DateTime luokkaa. Nämä toiminnot tarjoavat muita mahdollisuuksia päivämäärien käsittelyyn, kuten erilaisten muotoilujen tekemiseen. 

## Katso myös: 
- [Microsoftin PowerShellin viralliset dokumentaatiot](https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.1)
- [PowerShell Wikin päivämäärän ja ajan käyttäminen](https://github.com/PowerShell/PowerShell/wiki/DateTime-and-TimeSpans)