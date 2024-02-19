---
aliases:
- /fi/powershell/finding-the-length-of-a-string/
date: 2024-01-20 17:48:13.302475-07:00
description: "Merkkijonon pituuden selvitt\xE4misess\xE4 saadaan tietoon kuinka monta\
  \ merkki\xE4 merkkijonossa on. T\xE4m\xE4 on hy\xF6dyllist\xE4 esimerkiksi datan\
  \ validoinnissa, tekstin\u2026"
lastmod: 2024-02-18 23:09:07.840357
model: gpt-4-1106-preview
summary: "Merkkijonon pituuden selvitt\xE4misess\xE4 saadaan tietoon kuinka monta\
  \ merkki\xE4 merkkijonossa on. T\xE4m\xE4 on hy\xF6dyllist\xE4 esimerkiksi datan\
  \ validoinnissa, tekstin\u2026"
title: "Merkkijonon pituuden selvitt\xE4minen"
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
