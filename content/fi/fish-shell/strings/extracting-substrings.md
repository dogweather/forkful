---
date: 2024-01-20 17:45:48.115249-07:00
description: "Mik\xE4 se on ja miksi se on tarpeellista? Merkkijonojen osien irrottaminen\
  \ \u2013 eli substringien erist\xE4minen \u2013 tarkoittaa tietyn merkkijonon aliosan\
  \ kaivamista\u2026"
lastmod: '2024-03-13T22:44:56.981151-06:00'
model: gpt-4-1106-preview
summary: "Mik\xE4 se on ja miksi se on tarpeellista."
title: Merkkijonojen osien poimiminen
weight: 6
---

## What & Why?
Mikä se on ja miksi se on tarpeellista? Merkkijonojen osien irrottaminen – eli substringien eristäminen – tarkoittaa tietyn merkkijonon aliosan kaivamista ja käsittelyä. Ohjelmoijat tekevät tätä datan jäsentämiseen, esimerkiksi tiedostonimistä päätteiden poistamiseen tai päivämäärien muotoiluun.

## How to:
Fish Shellillä substringien käsittely onnistuu esimerkiksi `string`-komennon avulla:

```Fish Shell
# Esimerkki: Ota merkkijonosta "MeriTaimen" osa "Taimen"
set original "MeriTaimen"
set substring (string sub -s 5 $original)
echo $substring
```
Tuloste:
```
Taimen
```

```Fish Shell
# Esimerkki: Ota viimeiset 6 merkkiä merkkijonosta
set filename "photo_2023-03-28.jpg"
set extension (string sub -e 6 $filename)
echo $extension
```
Tuloste:
```
28.jpg
```

## Deep Dive
Fish Shell on uudehko tulokas komentotulkkeihin nähden, verrattuna vaikkapa BASHiin. Se on suunniteltu interaktiiviseen käyttöön ja helppokäyttöisyyteen unohtamatta ohjelmointiominaisuuksia. Substringien eristäminen `string`-komennolla on hyvä esimerkki Fishin selkeästä syntaksista.

Ennen `string`-komentoa Fishissa täytyi käyttää yhdistelmiä kuten `sed` tai `awk`, mikä vaati enemmän käskyn ymmärtämistä ja muistamista. `string`-komennon tullessa kuvioihin homma yksinkertaistui huomattavasti.

Ohjelmoijilla on muitakin keinoja substringien käsittelyyn esimerkiksi Pythonin tai Rubyn avulla, mutta kun työskennellään suoraan komentorivillä, Fish tarjoaa nopean ja tehokkaan tavan.

## See Also
Muita resursseja ja tietolähteitä:

- Fish Shellin viralliset dokumentit: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Tarkempi selostus `string`-komennosta: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Stack Overflow -keskustelu substringeistä Fishissä: [https://stackoverflow.com/questions/tagged/fish](https://stackoverflow.com/questions/tagged/fish)
