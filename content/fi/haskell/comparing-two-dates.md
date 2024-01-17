---
title:                "Kahden päivämäärän vertailu"
html_title:           "Haskell: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Päivämäärien vertailu on yksinkertaisesti kahden päivämäärän välisen eron laskemista. Tämä on hyödyllistä esimerkiksi aikaleimojen käsittelyssä tai tapahtumien järjestämisessä aikajärjestykseen. Ohjelmoijat tekevät tätä usein tarpeen mukaan käsitellessään päivämääriä.

## Kuinka?
Päivämäärien vertailu on helppo toteuttaa Haskellissa käyttämällä standardikirjaston tarjoamaa ```diffDays```-funktiota. Tämä toiminto ottaa kaksi päivämäärää parametreinaan ja palauttaa niiden välisen päivien määrän. Alla on esimerkki koodista ja tulostaa päivien määrän:

```Haskell
import Data.Time
date1 = fromGregorian 2021 3 1
date2 = fromGregorian 2021 4 1
diffDays date1 date2 -- tulostaa 31
```

## Sukellus syvälle
Päivämäärien vertailua käytetään jo kauan ennen tietokoneita, jolloin päiviä laskettiin käsin tai käyttämällä mekaanisia laitteita. Ohjelmistoalalla on myös muita tapoja käsitellä ja vertailla päivämääriä, kuten Unix-timestampien käyttö tai päivämääräobjektien vertaaminen.

Haskelissa päivämäärien vertailu perustuu Gregoriaaniseen kalenteriin, mutta myös muita kalentereita voidaan käyttää mukauttamalla päivämäärä-tyyppejä.

## Katso myös
Lisätietoja päivämäärien vertailusta Haskellissa löytyy [Haskellin dokumentaatiosta](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html#g:5).

Voit myös tutustua muihin päivämäärätietoista kertoviin artikkeleihin, kuten [täsmällisempi päivämäärien vertailu](https://www.iki.fi/o/zen/archives/2004/01/datetimecomparison.html) ja [päivämääräobjektien käyttö eri ohjelmointikielissä](https://en.wikipedia.org/wiki/Comparison_of_programming_languages_(date_and_time)).