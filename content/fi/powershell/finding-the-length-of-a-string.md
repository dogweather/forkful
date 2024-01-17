---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "PowerShell: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Stringin pituuden löytäminen tarkoittaa yksinkertaisesti merkkijonon merkkien lukumäärän laskemista. Tämä on tärkeä ohjelmoinnin toiminto, sillä se auttaa meitä ymmärtämään, kuinka monta merkkiä jokin teksti sisältää. Tämä voi olla hyödyllistä muun muassa datan käsittelyssä ja tietokantojen hallinnassa.

## Miten:
```PowerShell
$teksti = "Hei maailma!"
$tekstinPituus = $teksti.Length
Write-Host "Teksti sisältää $tekstinPituus merkkiä."
```
**Tulos:** Teksti sisältää 12 merkkiä.

## Syväsukellus:
Stringin pituuden löytämiseen voi käyttää myös muita menetelmiä, kuten for-loopia tai regex-komentoja. Tämä toiminto on yleisesti saatavilla useimmissa ohjelmointikielissä ja on melko helppo toteuttaa. Joskus saattaa olla tarpeen löytää stringin pituus myös ilman olemassa olevia toimintoja, ja tällöin voi olla hyödyllistä käyttää esimerkiksi pisteohjelmointia tai merkkitaulukoita.

## Katso myös:
- [Microsoftin ohjeet stringin pituuden laskemiseen PowerShellissä](https://docs.microsoft.com/en-us/powershell/scripting/samples/count-the-number-of-characters-in-a-string?view=powershell-7)
- [PowerShellin perusteet tutorial-videolla](https://www.youtube.com/watch?v=OK-FJDj5Dc0)