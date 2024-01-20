---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärän muuntaminen merkkijonoksi PowerShellissa tarkoittaa päivämääräobjektin esittämistä luettavassa muodossa. Tätä tehdään, jotta päivämäärät olisi helpompi tallentaa ja käyttää eri sovelluksissa.

## Miten näin:

Tässä on yksinkertainen esimerkki miten se tehdään:

```PowerShell
$pvm = Get-Date
$pvmmerkkijonona = $pvm.ToString("dd.MM.yyyy")

#Tulostaa esimerkiksi: 12.07.2022
Write-Host $pvmmerkkijonona
```
Joten seuraavassa esimerkissä käytämme `Get-Date` komentoa saadaksemme nykyisen päivämäärän ja `.ToString()` funktiota muuntaaksemme sen merkkijonoksi.

## Syvempi sukellus:

Historiallisesti päivämäärän muuntamista merkkijonoiksi on käytetty monissa sovelluksissa, kuten tietokannoissa ja tiedostojen nimissä, jotta tiedot saadaan järjestettyä ja löydettäväksi. 

Vaihtoehtoisesti voit määrittää DateTime-objekteja yksilöllisillä muotoiluilla käyttämällä `.ToString("muoto")` -metodia. Esimerkiksi, `.ToString("dd-MM-yyyy HH:mm:ss")` palauttaa merkkijonon muodossa "12-07-2022 14:30:00".

Muunnoksen toteuttamisessa käytetään .NET Frameworkin ja .NET Coren DateTime-tyypin ToString-metodia, joka muuntaa päivämäärän ja ajan arvot erilaisiksi merkkijonoformaateiksi.

## Katso myös:

[Microsoftin virallinen PowerShell-dokumentaatio](https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.1)

[Microsoft .NETin DateTime-kirjaston dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)