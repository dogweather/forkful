---
title:    "Gleam: Alimerkkijonojen erottaminen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi: Miksi haluaisit erottaa tekstipätkiä?

Erillisten tekstipätkien erottaminen on tärkeä osa ohjelmointia monessa tilanteessa. Se voi helpottaa tiettyjen tietojen löytämistä tekstimuotoisista tiedostoista tai helpottaa koodin jäsentämistä paloiksi. Gleam-ohjelmointikielellä on kätevä tapa tehdä tämä: substr (erottaminen). Tässä artikkelissa tarkastelemme tarkemmin, miten substr toimii ja miten sitä voi käyttää.

## Miten: Koodiesimerkkejä ja esimerkkipäätulo "```Gleam ... ```" koodilohkoissa.

### Esimerkki 1:
```Gleam
"Hello Gleam!"
|> String.substr(6,5)
```

Tulostaa: "Gleam"

### Esimerkki 2:
```Gleam
"123456789"
|> String.substr(3,2)
```

Tulostaa: "34"

### Esimerkki 3:
```Gleam
"Kissat ovat ihania"
|> String.substr(7,5)
```

Tulostaa: "ovat "

Kuten näet esimerkeistä, substr ottaa parametreikseen merkkijonon, josta halutaan erottaa osa, sekä aloitus- ja lopetuskohdan. Lopputulos on uusi merkkijono, joka sisältää vain halutun osan alkuperäisestä merkkijonosta. Tämä on erittäin hyödyllinen, jos haluat esimerkiksi lukea tiettyjä merkkitietoja tiedostosta tai haluat jäsentää koodiasi erillisiksi osiksi.

## Syvempää tietoa erottavista tekstipätkistä

Gleam-ohjelmointikielessä on myös muita tapoja erottaa tekstipätkiä. Yksi vaihtoehto on käyttää Erlangin standardikirjaston funktiota `string:substr`, joka toimii samalla tavalla kuin Gleamin `String.substr`. Lisäksi on olemassa myös `binary:part`-funktio, joka pystyy käsittelemään myös muita tietotyyppejä, kuten binäärejä.

On myös hyvä huomata, että substr ei muuta alkuperäistä merkkijonoa, vaan palauttaa uuden merkkijonon. Tämä on tärkeää muistaa, jos haluat esimerkiksi käyttää substr sijoittamaan uutta tietoa merkkijonoon.

## Katso myös

- Gleam dokumentaatio: https://gleam.run/
- Erlang string- ja binary-moduulit: http://erlang.org/doc/man/string.html http://erlang.org/doc/man/binary.html