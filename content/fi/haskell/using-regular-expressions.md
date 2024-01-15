---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Haskell: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Haluatko muokata tekstejäsi tavalla, joka säästää aikaa ja vaivaa? Sisällöntuottajat, ohjelmoijat ja tietojenkäsittelijät voivat hyödyntää säännöllisiä lausekkeita (regular expressions) kaikenlaisiin tekstikäsittelytehtäviin.

## Miten

Säännölliset lausekkeet ovat joukko merkkijonoja, jotka mahdollistavat tarkkojen hakuehtojen määrittämisen. Käytä yleistä RegExp-kirjastoa lisätäksesi säännöllisiä lausekkeita Haskell-koodiisi. Katso esimerkiksi seuraavaa koodinpätkää ja sen tuottamaa tulostetta.

```Haskell
import Text.Regex.PCRE

-- Valitaan regex-yhteensopiva merkkijono
s = "Tämä on esimerkkilause 123"

-- Etsitään kaikkia numeroyhdistelmiä merkkijonosta
tulokset = s =~ "[0-9]+"

-- Tulostetaan löydetyt numerot
print tulokset
```
Tulostaa: ["123"]

## Syventymistä

Nyt kun tiedät, kuinka luoda säännöllisiä lausekkeita Haskellilla, voit tutustua erilaisiin ominaisuuksiin ja toimintoihin, kuten tiettyjen merkkien korvaamiseen, usean haun suorittamiseen samanaikaisesti ja paljon muuhun. Voit lukea lisää säännöllisistä lausekkeista ja niiden käytöstä esimerkiksi seuraavilta sivuilta:

- [WikiBooks](https://en.wikibooks.org/wiki/Haskell/Regular_expressions)
- [Haskell.org](https://www.haskell.org/hoogle/?hoogle=regex)

## Katso myös

[Säännölliset lausekkeet Cheat Sheet (englanniksi)](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)