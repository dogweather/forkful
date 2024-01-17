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

## Mitä ja miksi?

Säännöllisillä lausekkeilla (regular expressions) tarkoitetaan tekstinkäsittelyyn tarkoitettuja ilmaisuja, jotka kuvaavat tiettyjä merkkijonoja tai merkkijonojen joukkoja. Näitä ilmaisuja käytetään usein ohjelmointitehtävissä, kuten tekstien haussa ja korvaamisessa. Säännölliset lausekkeet helpottavat näiden tehtävien suorittamista ja säästävät aikaa ja vaivaa.

## Miten:

Haskellissa säännölliset lausekkeet ovat osa kirjastoa nimeltään "Text.Regex". Niiden käyttö vaatii kirjaston tuomisen näkyville, jonka jälkeen voimme aloittaa ilmaisujen käytön esimerkiksi seuraavasti:

```Haskell
import Text.Regex

teksti = "Tervetuloa Haskelliin!"

match "Haskell" teksti
```
Tulos olisi ```Just ("Haskell", "")```, koska tekstissä esiintyy sana "Haskell" ja se vastaa säännöllistä lauseketta.

Toinen esimerkki, jossa säännöllisiä lausekkeita käytetään korvaamiseen:
```Haskell
replace "Haskell" "Hask" teksti
```
Tulos olisi sana "Tervetuloa Haskiin!", koska lauseke korvattiin uudella sanalla.

## Syväsukellus:

Säännölliset lausekkeet ovat peräisin matematiikasta ja ovat kehitetty tekstien käsittelyyn jo 1950-luvulla. Nykyään lähes jokaisella ohjelmointikielellä, mukaan lukien Haskell, on mahdollisuus käyttää säännöllisiä lausekkeita. On myös muita vaihtoehtoja, kuten rekursiivinen tekstien käsittely, mutta säännölliset lausekkeet ovat yleisesti hyväksytty ja tehokas tapa suorittaa näitä tehtäviä.

Sisäisesti säännölliset lausekkeet käännetään deterministisiksi automaateiksi, jotka suorittavat vertailut ja korvaukset. Ne ovat siten nopeampia kuin manuaalinen tekstien käsittely koodilla.

## Katso myös:

- [Haskellin dokumentaatio säännöllisistä lausekkeista](https://hackage.haskell.org/package/regex-base)
- [RegExr - interaktiivinen säännöllisten lausekkeiden testaus- ja oppimisalusta](https://regexr.com/) 
- [Säännöllisten lausekkeiden perusteet - opetusvideo (englanniksi)](https://www.youtube.com/watch?v=5hjLQVFLDs4)