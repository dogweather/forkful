---
title:                "Merkkijonojen yhdistäminen"
html_title:           "TypeScript: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

##

## Mitä ja miksi?

Kokonaisuudessaan ottaen, merkkijonojen yhdistäminen tarkoittaa kahden tai useamman merkkijonon yhdistämistä yhdeksi merkkijonoksi. Se on yleinen ohjelmoinnin käytäntö, joka helpottaa merkkijonojen käsittelyä ja muotoilua. Se on erityisen hyödyllistä, kun haluat luoda dynaamisia tekstejä, jotka sisältävät muuttujien ja vakioiden arvoja.

## Kuinka se tehdään?

Yhdistämistä voi tehdä käyttämällä plus-merkkiä (+) ja kaksi pistettä (:) merkkijonojen välillä. Esimerkiksi:

```TypeScript
let etunimi = "Matti";
let sukunimi = "Meikäläinen";
let kokonimi = etunimi + " " + sukunimi;

console.log(kokonimi);
// tulostaa "Matti Meikäläinen"
```

Voit myös yhdistää merkkijonon ja muuttujan tai vakioiden arvoja suoraan yhteen merkkijonoon käyttämällä kaksi pistettä. Esimerkiksi:

```TypeScript
let ikä = 25;
let tervehdys = "Hei, olen " + ikä + ". vuotias.";

console.log(tervehdys);
// tulostaa "Hei, olen 25. vuotias."
```

## Syvemmälle syövereihin

Merkkijonojen yhdistämisestä puhuttaessa on tärkeää muistaa, että se ei ole ainoa tapa käsitellä ja muokata tekstejä. Jotkut kielet, kuten Python, tarjoavat kattavammat merkkijonojen käsittelytoiminnot verrattuna TypeScriptin yksinkertaiseen yhdistämiseen.

Toinen asia, jonka on hyvä pitää mielessä, on merkkijonojen yhdistämisen tehokkuus. Jos yhdistät suuren määrän merkkijonoja, se voi hidastaa ohjelman suoritusta. Tässä tapauksessa on parempi käyttää tarkempia työkaluja, kuten StringBuilder.

Merkkijonojen yhdistämiseen käytetään usein myös erityisiä yhdistämisfunktioita, jotka mahdollistavat monimutkaisempien muotoilujen ja muuttujien käytön. Ota siis selvää, mitä vaihtoehtoja ja työkaluja kielesi tarjoaa merkkijonojen käsittelyyn.

## Katso myös

Mikäli haluat oppia lisää merkkijonojen yhdistämisestä TypeScriptissä, suosittelemme lukemaan TypeScriptin virallisen dokumentaation osion [Merkkijonokäsittely](https://www.typescriptlang.org/docs/handbook/basic-types.html#string-handling). Myös esimerkkikoodien läpikäyminen ja kokeileminen auttavat ymmärtämään paremmin kyseistä aihealuetta.