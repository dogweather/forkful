---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
PowerShellilla päivämäärän hakeminen tarkoittaa tarkalleen sitä miltä se kuulostaakin: saat tietoonsa tämänhetkisen päivämäärän ja kellonajan. Tämä on hyödyllistä aikaleimojen luomisessa, tapahtumien ajoittamisessa tai ohjelman suoritusajasta seuraamisessa.

## Miten se tehdään:
Haetaan päivämäärä PowerShellilla näin:

```PowerShell
$NykyinenPäivämäärä = Get-Date
Write-Output $NykyinenPäivämäärä
```

Näin saat tulostettua nykyisen päivämäärän ja kellonajan. Esimerkki tulosteesta saattaa näyttää tällaiselta:

```PowerShell
maanantai, 1. marraskuuta 2021 12:00:00
```

## Syvällisempi tieto
PowerShellilla päivämäärän hakeminen on ollut mahdollista sen julkaisusta lähtien, ja se on pysynyt melko muuttumattomana. Tosin, on olemassa erilaisia tapoja hakea päivämäärätietoja PowerShellilla. Esimerkiksi, voit määrittää tietyn muodon päivämäärälle käyttäen "ToShortDateString()" tai "ToLongTimeString()" metodeja.

Implementointiyksityiskohdista, `Get-Date` cmdlet palauttaa nykyisen päivämäärän ja kellonajan objektina, joka on System.DateTime tyyppiä. Se sisältää valtavan määrän tietoja, kuten vuoden, kuukauden, viikonpäivän ja kellonajan sekunnin tarkkuudella.

## Katso myös
Lisätietoja PowerShellin Get-Date cmdletistä, päivämäärän muotoilusta ja muista päivämäärätoiminnoista löydät seuraavista lähteistä:

- Microsoftin virallinen dokumentaatio Get-Date cmdletistä: 
  [Täältä](https://docs.microsoft.com/fi-fi/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1).
  
- Yksityiskohtainen opas päivämäärän ja ajan käsittelystä PowerShellissa: 
  [Täältä](https://devblogs.microsoft.com/scripting/weekend-scripter-working-with-powershell-date-commands/). 

- Sovelluksia päivämäärälle ja aikaleimoille PowerShellissa: 
  [Täältä](https://4sysops.com/archives/use-powershell-to-work-with-date-and-time/).