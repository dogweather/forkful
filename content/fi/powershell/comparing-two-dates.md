---
title:                "Kahden päivämäärän vertailu"
html_title:           "PowerShell: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?
Päivämäärien vertailu on tärkeä osa ohjelmointia, sillä se mahdollistaa ajallisten tietojen tarkastelun ja käsittelyn. Esimerkiksi eri tilausten toimituspäivämäärien vertailu auttaa seuraamaan tuotteiden saapumista ja varastonhallintaa.

## Miten:
```PowerShell
$date1 = Get-Date -Year 2020 -Month 08 -Day 15
$date2 = Get-Date -Year 2020 -Month 08 -Day 20

Write-Host "Ensimmäinen päivämäärä: $date1"
Write-Host "Toinen päivämäärä: $date2"
Write-Host "Onko ensimmäinen päivämäärä ennen toista? $date1 -lt $date2"
```

Tulostus:

```
Ensimmäinen päivämäärä: 15.8.2020 00:00:00
Toinen päivämäärä: 20.8.2020 00:00:00
Onko ensimmäinen päivämäärä ennen toista? True
```

## Syväsukellus:
Päivämäärien vertailu on ollut tärkeä osa ohjelmointia jo vuosien ajan. Aikaisemmin se oli tehtävä manuaalisesti, mutta nykyään PowerShell tarjoaa helpon ja nopean tavan suorittaa vertailuja. On myös mahdollista vertailla päivämääriä eri muodoissa, kuten merkkijonoina.

Vaihtoehtoisia tapoja vertailla päivämääriä ovat esimerkiksi käyttääkseen `Where-Object` komentoa tai DateTime-objekteja C#-koodin sisällä.

Vertailun toteuttamisessa tulee ottaa huomioon päivämäärien aikavyöhykkeet ja muotoilu, jotta vertailu on mahdollisimman tarkka ja luotettava.

## Katso myös:
- [Get-Date cmdlet](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7)
- [DateTime-rakenne C#-koodissa](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1)
- [PowerShell vertailukomennot](https://docs.microsoft.com/en-us/powershell/scripting/samples/working-with-dates?view=powershell-7)