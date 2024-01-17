---
title:                "Lauseiden yhdistäminen"
html_title:           "Gleam: Lauseiden yhdistäminen"
simple_title:         "Lauseiden yhdistäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Merkkijonojen linkittäminen on tapa yhdistää kaksi tai useampaa merkkijonoa yhdeksi. Tämä on hyödyllistä esimerkiksi taulukoissa tai viesteissä, joissa tarvitaan dynaamista dataa. Ohjelmoijat käyttävät sitä helpottamaan muutosten tekemistä ja tietojen hallintaa.

## Miten:
```Gleam
let etunimi = "Matti"
let sukunimi = "Meikäläinen"

merkkijono.concat(etunimi, " ", sukunimi)
//tulostaa "Matti Meikäläinen"
```

## Syvemmälle:
Merkkijonojen yhdistäminen on ollut käytössä jo pitkään ohjelmoinnissa, ja siihen on kehitetty erilaisia menetelmiä kuten interpolointi ja muuttujien sisällyttäminen merkkijonoihin. Muita tapoja yhdistää merkkijonoja ovat muun muassa string.format ja string.concat. Gleamin merkkijonojen yhdistämisfunktio on optimoitu ja nopea, joten sitä kannattaa käyttää tarpeen mukaan.

## Katso myös:
Lisää tietoa Gleamin merkkijonojen käsittelyyn löydät dokumentaatiosta: https://gleam.run/documentation/strings/concat. Voit myös tutustua muihin tapoihin yhdistää merkkijonoja muiden ohjelmointikielten, kuten Javascriptin tai Pythonin, avulla.