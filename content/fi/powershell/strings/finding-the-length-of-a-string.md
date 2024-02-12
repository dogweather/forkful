---
title:                "Merkkijonon pituuden selvittäminen"
aliases: - /fi/powershell/finding-the-length-of-a-string.md
date:                  2024-01-20T17:48:13.302475-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon pituuden selvittäminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkkijonon pituuden selvittämisessä saadaan tietoon kuinka monta merkkiä merkkijonossa on. Tämä on hyödyllistä esimerkiksi datan validoinnissa, tekstin käsittelyssä ja käyttöliittymän suunnittelussa.

## Miten toimitaan:
```PowerShell
# Luo merkkijono-muuttuja
$merkkijono = "Hei, PowerShell!"

# Etsi merkkijonon pituus
$pituus = $merkkijono.Length

# Tulosta pituus
Write-Output $pituus
```
Tuloste: 
```
15
```

## Syventävä tieto:
PowerShellissa merkkijonon pituuden selvittäminen on yksinkertaista käyttäen `.Length`-ominaisuutta, joka on ollut käytössä ohjelmointikielissä jo vuosikymmeniä. Historiallisesti, perinteisissä ohjelmointikielissä, kuten C:ssä, pituuden selvittäminen saattoi olla työläämpää, sillä kehittäjän täytyi itse kierrellä merkkijonoa loppumerkin löytämiseksi. PowerShellissa, kuten monissa nykykielissä, merkkijonot ovat olioita, joilla on valmiita metodeja ja ominaisuuksia.

Vaihtoehtoisia tapoja löytyy, kuten käyttämällä `Measure-Object` komentoa:

```PowerShell
$merkkijono | Measure-Object -Character | Select-Object -ExpandProperty Characters
```

Tämän funktionaalisen lähestymistavan sijaan `.Length` on kuitenkin yleensä suositumpi ja suoraviivaisempi, sillä se on suora ominaisuuskutsu ilman putkittamista. Käytännössä `.Length` on myös tehokkaampi, sillä se on optimoitu toiminto, joka on laajalti dokumentoitu ja yleisesti tunnettu kehittäjien keskuudessa.

## Katso myös:
- String-metodien käyttö PowerShellissa: [String Methods - PowerShell](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-6.0#methods)
- PowerShellin perusteet: [PowerShell 101](https://channel9.msdn.com/Series/GetStartedPowerShell3)
