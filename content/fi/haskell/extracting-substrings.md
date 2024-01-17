---
title:                "Alimerkkijonojen erottaminen"
html_title:           "Haskell: Alimerkkijonojen erottaminen"
simple_title:         "Alimerkkijonojen erottaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Haluatko jakaa merkkijonon pienempiin osiin? Tämä prosessi tunnetaan nimellä "alamerkkijonojen erottelu" ja se on hyödyllistä, kun haluat manipuloida tai analysoida tiettyjä osia tekstistä. Ohjelmoijat käyttävät tätä tekniikkaa monissa tilanteissa, kuten tekstinkäsittelyssä tai tiedon käsittelyssä.

## Näin teet sen:
Haskellin "Data.List"-kirjastossa on valmis funktio nimeltä "take", joka ottaa listan tai merkkijonon sekä numeron ja palauttaa halutun määrän alkioita alusta lähtien. Voit käyttää tätä ominaisuutta redusoimaan merkkijonoa osaksi pienempiä osia. Katso alla oleva esimerkki:

```Haskell
import Data.List (take)

substring = take 5 "Tämä on yksi testi."
```

Tulostus: "Tämä "

## Syvä sukellus:
"Substring" on tekniikka, joka esiintyy useissa ohjelmointikielissä ja se on ollut käytössä jo vuosikymmenten ajan. Joissain kielissä, kuten C: ssä, alamerkkijonot ovat oma tietotyyppi. On myös muita tapoja jakaa merkkijonoja, kuten käyttämällä listoja ja pätsien operaattoreita.

Implementaatiomme käyttää takan funktiota, mutta voit myös käyttää muita tapoja, kuten subList-funktion tekemistä, joka ottaa listan ja alku- ja loppu-indeksin ja palauttaa välillä olevat alkiot. Voit myös kirjoittaa oman funktion, joka toimii samalla tavalla kuin takan funktio. Vaihtoehdot ja implementointitavat voivat vaihdella riippuen ohjelmointikielestä ja tilanteesta.

## Katso myös:
- Data.List - Haskellin virallinen dokumentaatio
- Merkkijonon käsittely - Wikipedian artikkeli alamerkkijonoista ja niiden käytöstä.