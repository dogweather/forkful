---
title:                "Elm: Alimerkkijonojen erottaminen"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi: Miksi tutkia osajonon poimimista
Kun käsittelet suuria määriä tekstiä, voi joskus olla tarpeen poimia siitä tiettyjä osajonoja. Tämä voi esimerkiksi auttaa tietokantojen suodattamisessa tai käyttäjän syötteiden validoinnissa.

## Kuinka: Esimerkkejä osajonojen poimimisesta Elm-ohjelmassa
```Elm
import String

teksti = "Tämä on esimerkkiteksti Elm-ohjelmassa"

osajono = String.slice 10 18 teksti

main = 
    text osajono

-- Output:
-- "on esimerk"
``` 

Voit myös käyttää `String.split` funktiota, joka pilkkoo merkkijonon tietyistä erotinpisteistä ja palauttaa listan osajonoja. Seuraavassa esimerkissä pilkotaan syöte välilyönnin kohdalta ja palautetaan vain ensimmäinen osajono.

```Elm
import String

syote = "Kissa koira lehmä hevonen"

osat = String.split " " syote

ekaOsajono = List.head osat

main =
    text ekaOsajono

-- Output:
-- "Kissa"
```

## Syvempi sukellus: Lisätietoja osajonojen poimimisesta
`String.slice` ja `String.split` ovat hyödyllisiä funktioita osajonojen poimimiseen, mutta niiden käyttö vaatii ymmärrystä syötteiden ja erotinpisteiden välisen suhteen sekä merkkijonon käsittelyn perusteista. On myös tärkeää huomioida merkkijonon indeksien laskenta ja tyhjien osajonojen käsittely.

## Katso myös
- [Elm:n virallinen dokumentaatio merkkijonojen käsittelystä](https://elm-lang.org/docs/from-javascript-strings)
- [Merkkijonojen pilkkomisen teoriatausta](https://en.wikipedia.org/wiki/String_(computer_science)#General_delimited_formats)