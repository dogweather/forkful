---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "PowerShell: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärän muuntaminen merkkijonoksi on tärkeä osa ohjelmointia, jota monet ohjelmoijat tekevät päivittäin. Tämä tarkoittaa päivämäärän esittämistä numeerisessa muodossa tekstimuodossa, esimerkiksi "1.1.2020" sijaan "tammikuu 1, 2020". Tätä tarvitaan esimerkiksi tietokannoissa, tiedostojen nimissä ja muissa ohjelmoinnin osa-alueilla, joissa päivämääriä käsitellään.

## Kuinka tehdä se:
```PowerShell
$date = Get-Date
[string]$formattedDate = $date.ToString("dd.MM.yyyy")

Write-Host $formattedDate
```
Tämä koodi ottaa nykyisen päivämäärän PowerShellissa ja muuntaa sen merkkijonoksi "dd.MM.yyyy" -muodossa (päivä.kuukausi.vuosi). Tämän jälkeen se tulostaa merkkijonon komentokehotteeseen.

**Tuloste:** 10.12.2020

## Syvemmälle:
Päivämäärän muuntaminen merkkijonoksi on ollut tärkeä prosessi jo pitkään, johtuen eri maiden ja kulttuurien tapoja esittää päivämääriä. Toinen tapa tehdä tämä PowerShellissa on käyttää `-format`-parametria. Lisäksi on olemassa muita tapoja muuttaa päivämäärä eri muodoissa, kuten "pvm" tai "ddmmyyyy". Päivämäärän muuntaminen merkkijonoksi voi myös sisältää aikakomponentin.

## Katso myös:
- [PowerShell Help: Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [PowerShell Help: ToString Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring?view=net-5.0)