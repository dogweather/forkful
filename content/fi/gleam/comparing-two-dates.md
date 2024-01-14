---
title:    "Gleam: Kahden päivämäärän vertailu"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

Blogi: Miksi vertailla kahta päivämäärää

Olet ehkä huomannut, että usein ohjelmissa täytyy verrata kahta eri päivämäärää. Tämä voi johtua esimerkiksi siitä, että haluat tarkistaa, onko tietty tapahtuma jo tapahtunut vai ei. Gleam-kielinen vertailu tarjoaa helpon ja tehokkaan tavan tehdä tämä.

## Miten

Vertailu kahden päivämäärän välillä Gleamilla onnistuu käyttämällä `Date.compare` -funktiota. Tämä funktio ottaa kaksi päivämäärää parametreinä ja palauttaa yhtäsuuruusoperaattorin arvon (-1, 0 tai 1) vertailemalla niiden päivämääräarvoja.

```
Gleam -koodiesimerkki:

let date_1 = Date.new({ year: 2021, month: 9, day: 15 })
let date_2 = Date.new({ year: 2021, month: 9, day: 20 })

let result = Date.compare(date_1, date_2)

gleam_assert.equal(result, -1)
```

Tässä esimerkissä olemme luoneet kaksi eri päivämäärää ja vertailleet niitä käyttämällä `Date.compare` -funktiota. Koska `date_1` on pienempi kuin `date_2`, funktio palauttaa arvon -1.

## Syvempi sukellus

Gleamilla on myös muita käteviä toimintoja, joita voi käyttää vertailemaan päivämääriä. `Date.is_before` ja `Date.is_after` -funktiot tarkistavat, onko kyseinen päivämäärä ennen vai jälkeen toista.

```
Gleam-koodiesimerkki:

let date_3 = Date.new({ year: 2021, month: 9, day: 1 })

let result_1 = Date.is_before(date_3, date_1)
let result_2 = Date.is_after(date_3, date_2)

gleam_assert.equal(result_1, true)
gleam_assert.equal(result_2, false)
```

Tässä esimerkissä olemme tarkistaneet, onko `date_3` ennen `date_1` ja jälkeen `date_2`. Koska `date_3` sijoittuu näiden kahden päivämäärän välille, `Date.is_before` -funktio palauttaa arvon true ja `Date.is_after` arvon false.

## Katso myös

- Gleam-virallinen dokumentaatio: https://gleam.run/
- Vertailu ja yhtäsuuruusoperaattori: https://gleam.run/book/compare.html
- Päivämääräarvot Gleamissa: https://gleam.run/book/date.html