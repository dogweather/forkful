---
date: 2024-01-20 17:48:13.302475-07:00
description: "Miten toimitaan: PowerShellissa merkkijonon pituuden selvitt\xE4minen\
  \ on yksinkertaista k\xE4ytt\xE4en `.Length`-ominaisuutta, joka on ollut k\xE4yt\xF6\
  ss\xE4\u2026"
lastmod: '2024-04-05T22:51:10.924081-06:00'
model: gpt-4-1106-preview
summary: "PowerShellissa merkkijonon pituuden selvitt\xE4minen on yksinkertaista k\xE4\
  ytt\xE4en `.Length`-ominaisuutta, joka on ollut k\xE4yt\xF6ss\xE4 ohjelmointikieliss\xE4\
  \ jo vuosikymmeni\xE4."
title: "Merkkijonon pituuden selvitt\xE4minen"
weight: 7
---

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
