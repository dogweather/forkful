---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Elm: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Päivämäärän muuntaminen merkkijonosta on tapa, jolla ohjelmoijat voivat muuttaa tekstiä mahdolliseksi päivämääräksi. Tämä on hyödyllistä esimerkiksi, kun käyttäjä antaa päivämäärän syötteeksi tekstiformaatissa. Tällöin ohjelmoija voi muuntaa sen sopivaan muotoon käsitelläkseen sitä sovelluksessa.

# Miten?

```Elm
Date.fromIsoString "2020-06-29"
```
Tämä esimerkki käyttää Elm:n Date-moduulia muuntamaan merkkijonon ISO 8601 -päivämääräksi. 
```
Ok (Date.fromIsoString "2020-06-29")  --> Ok (Date.fromTime 1593417600000)
```
Elm palauttaa päivämäärän tietyn aikaleiman muodossa, joka voidaan helposti muuttaa näytöllä näytettävään muotoon.

# Syvempi sukellus

Päivämäärän muuntaminen merkkijonosta ei ole uusi käsite. Jo 1970-luvulla monet ohjelmointikielet, kuten BASIC ja COBOL, sisälsivät toimintoja päivämäärän käsittelyyn. Nykyään on myös muita tapoja käsitellä päivämääriä, kuten käyttämällä ulkoisia kirjastoja.

# Katso myös

Lisätietoa Elm:n Date-moduulista löytyy [viralliselta verkkosivustolta](https://package.elm-lang.org/packages/elm/time/latest/). Ohjelmoijan kannattaa myös tutustua muihin tapoihin käsitellä päivämääriä, kuten Moment.js-kirjastoon.