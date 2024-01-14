---
title:                "Haskell: Tekstin etsiminen ja korvaaminen"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointiprojekteissa joudutaan käsittelemään suuria määriä tekstiä. Olipa kyseessä sitten tiedostojen käsittely tai tietokantojen hakeminen, tekstin hakeminen ja korvaaminen on usein välttämätöntä. Haskell-ohjelmointikieli tarjoaa helpon ja tehokkaan tavan tehdä tätä, mikä tekee siitä suosikin monien kehittäjien keskuudessa.

## Kuinka tehdä

Haskellissa tekstin hakeminen ja korvaaminen tapahtuu ```substitute```-funktion avulla. Tämän funktion ensimmäinen parametri on haettava teksti ja toinen parametri on korvaava teksti. Alla on esimerkki, jossa korvaamme kaikki esiintymät merkkijonossa "Hello" merkkijonalla "Hei".

```Haskell
substitute "Hello" "Hei" "Hello World!" 
```

Tämä tuottaa tuloksen ```Hei World!```, mikä vastaa odotuksiamme.

Jotta voimme käsitellä suurempaa määrää tekstiä, voimme käyttää ```replace```-funktiota, joka korvaa kaikki esiintymät haetusta tekstistä korvaavalla tekstillä. Tämä on hyödyllistä, kun meillä on esimerkiksi suuri tiedosto, jonka haluamme käsitellä.

```Haskell
replace "Hello" "Hei" "Hello World! Hello Universe!"
```

Tuloksena saamme ```Hei World! Hei Universe!``` mikä osoittaa, että molemmat esiintymät "Hello" on korvattu "Hei".

## Syvällinen sukellus

```substitute```- ja ```replace```-funktioiden lisäksi Haskell tarjoaa muita tapoja käsitellä tekstiä. Näitä ovat muun muassa ```strip```, joka poistaa merkit tekstin alusta ja lopusta, ```split```, joka jakaa tekstin listaksi merkkijonoja annetun erotinmerkin avulla, ja ```join```, joka yhdistää listan merkkijonomuotoon.

Useimmissa projekteissa nämä perusfunktiot riittävät tekstin käsittelyyn, mutta Haskellilla on myös mahdollista luoda omia, räätälöityjä toimintoja tarpeen mukaan.

## Katso myös

- [Haskellin dokumentaatio tekstinkäsittelyfunktioista](https://www.haskell.org/hoogle/?hoogle=Text+-%3E+Text+-%3E+Text)
- [Haskellin tekstinkäsittely- ja merkkchaini-ohjelmointi](http://book.realworldhaskell.org/read/text-processing-and-regular-expressions.html)