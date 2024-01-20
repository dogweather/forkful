---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Merkkijonojen yhdistäminen tarkoittaa useiden merkkijonojen liittämistä yhteen. Ohjelmoijat tekevät tämän yleensä, kun heidän on luotava dynaamisesti muotoiltuja merkkijonoja esimerkiksi käyttäjäviestejä tai loki-merkintöjä varten.

## Kuinka tehdä:
Gleam-ohjelmoinnissa voit yhdistää merkkijonoja käyttämällä "+" -operaattoria.

```Gleam
let viesti = "Hei" + ", maailma!"
io.println(viesti)  // Tulostaa: Hei, maailma!
```

## Syvä Sukellus
Merkkijonojen yhdistäminen on ollut ohjelmoinnissa yksinkertainen ja yleinen tehtävä alusta alkaen. Gleamissa "+" -operaattori yhdistää merkkijonoja luomalla uuden merkkijonon yhdistettyjen merkkijonojen alueelta muistissa. Yksi vaihtoehto on käyttää `concat` -funktiota List-moduulissa, joka liittää luettelossa olevat merkkijonot yhdeksi merkkijonoksi.

```Gleam
List.concat(["Hei", ", maailma!"])  // Palauttaa: "Hei, maailma!"
```

Kuitenkin suuren määrän merkkijonojen yhdistämisessä, "+" -operaattori saattaa olla tehoton käyttämän paljon muistia ja aikaa, koska se luo uuden merkkijono jokaisen operaation yhteydessä. Tässä tilanteessa `io.format/2` -funktion käyttäminen on tehokkaampaa.

```Gleam
let viesti = io.format("Hei, ~p!", ["maailma"])
io.println(viesti)  // Tulostaa: Hei, maailma!
```

## Katso Myös