---
title:                "Tekstitiedoston kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Tekstitiedoston kirjoittaminen tarkoittaa merkkijonojen tallentamista tiedostoon. Ohjelmoijat tekevät sitä datan tallentamiseen, lokien luontiin ja asetusten säilyttämiseen.

## How to:
PowerShellilla tiedoston kirjoittaminen on yksinkertaista. Käytä `Out-File`-komentoa tai lyhyempää `>` -operaattoria.

```PowerShell
# Käytä Out-File-komentoa
"Tämä on tekstisisältö" | Out-File -FilePath .\esimerkki.txt

# Tai käytä yksinkertaista uudelleenohjausta
"Tämä on toinen tekstirivi" > .\toinen_esimerkki.txt
```

Tulostiedosto `esimerkki.txt` ja `toinen_esimerkki.txt` luotu.

## Deep Dive:
PowerShell on Microsoftin kehittämä objektipohjainen komentosarjakirjasto ja komentotulkki. Se julkaistiin vuonna 2006 ja korvasi aiemmat Windowsin komentosarjatyökalut. Vaihtoehtoiset tavat tiedostojen kirjoittamiseen sisältävät `Set-Content`- ja `Add-Content` -komennot. Implementaation yksityiskohdat paljastavat, että `Out-File` käyttää .NET-luokkia tiedostojen käsittelyyn ja tukee merkistön koodausta ja rivinvaihtoja.

## See Also:
- PowerShellin `Out-File`-komento: [Out-File](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/out-file?view=powershell-7)
- PowerShellin `Set-Content` and `Add-Content` -komennot: [Set-Content](https://docs.microsoft.com/powershell/module/microsoft.powershell.management/set-content?view=powershell-7) | [Add-Content](https://docs.microsoft.com/powershell/module/microsoft.powershell.management/add-content?view=powershell-7)
