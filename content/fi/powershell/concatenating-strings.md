---
title:                "Joustopalatut merkkijonot"
html_title:           "PowerShell: Joustopalatut merkkijonot"
simple_title:         "Joustopalatut merkkijonot"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Miksi ohjelmoijat yhdistävät merkkijonoja? Merkkijonojen yhdistäminen tarkoittaa useiden merkkijonojen yhdistämistä yhdeksi kokonaisuudeksi. Tämä on hyödyllistä, kun haluat luoda uuden merkkijonon, joka sisältää tietoa useista eri lähteistä.

## Miten:

```PowerShell
# Esimerkki 1: Yksinkertainen yhdistäminen
$eka = "Tervetuloa "
$toka = "PowerShell-ohjelmointiin!"
$kokonaisuus = $eka + $toka
$kokonaisuus
```
```
Tervetuloa PowerShell-ohjelmointiin!
```

```PowerShell
# Esimerkki 2: Yhdistäminen muuttujien kanssa
$nimi = "Maria"
$ika = 25
$tervehdys = "Hei, olen " + $nimi + " ja minulla on " + $ika + " vuotta ikää."
$tervehdys
```
```
Hei, olen Maria ja minulla on 25 vuotta ikää.
```

```PowerShell
# Esimerkki 3: Yhdistäminen komentoriviltä saadun syötteen kanssa
$luku = Read-Host "Syötä luku:"
$viesti = "Mikään ei ole pysyvästi yhtä kuin " + $luku + "."
$viesti
```
```
Syötä luku: 42
Mikään ei ole pysyvästi yhtä kuin 42.
```

## Syvemmälle:

Merkkijonojen yhdistäminen on ollut käytössä jo kauan ennen PowerShell-kielen syntymistä. Monissa muissa kielissä käytetään erilaisia operaattoreita, kuten + tai &, merkkijonojen yhdistämiseen. Joissakin tilanteissa voi olla hyödyllistä käyttää niitä PowerShell-kielen sijaan.

## Katso myös:

- [Merkkijonojen yhdistäminen PowerShellissä - Microsoftin dokumentaatio](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_operators?view=powershell-7#-concatenation-operator)
- [Merkkijonojen yhdistäminen Pythonissa - Esimerkkejä ja vertailua muihin kieliin](https://realpython.com/python-string-concatenation/)