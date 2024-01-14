---
title:                "Gleam: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Satunnaislukujen generointi on tärkeä osa monia ohjelmia ja sovelluksia, joita käytämme päivittäin. Se voi auttaa arvonta- ja arpajaissovelluksissa, satunnaisen sisällön luomisessa tietokonepeleihin ja jopa satunnaisen datan luomisessa testaukseen. Gleam-ohjelmointikieli tarjoaa tehokkaat työkalut satunnaislukujen generointiin, mikä tekee siitä loistavan vaihtoehdon tähän tarkoitukseen.

## Kuinka

Gleam tarjoaa useita sisäänrakennettuja toimintoja satunnaislukujen luomiseen. Yksi yksinkertaisimmista tavoista on käyttää `random` -funktiota. Alla on esimerkki, jossa luodaan satunnainen kokonaisluku välillä 1-10:

```Gleam
let satunnainen = random.int(1, 10)
```

Tämä koodi palauttaa arvon, kuten 7 tai 2, jokainen suorituskerta. Voit myös generoida tietyn määrän satunnaislukuja kerrallaan `random.ints` -funktiolla:

```Gleam
let satunnaiset_luvut = random.ints(5, 1, 10)
```

Tämä koodi palauttaisi listan, jossa on viisi satunnaista kokonaislukua välillä 1-10, kuten esimerkiksi `[3, 9, 6, 4, 8]`.

## Syventävä tarkastelu

Gleam tarjoaa myös muita hyödyllisiä toimintoja satunnaislukujen generointiin. Voit esimerkiksi käyttää `random.float` -funktiota luodaksesi satunnaisen liukuluvun välillä 0.0-1.0. Jos haluat määrittää tarkan numeron desimaalipaikkoja, voit käyttää `random.float_with_precision` -funktiota. Voit myös luoda satunnaisen merkkijonon tietystä merkkien joukosta `random.from_chars` tai satunnaisen elementin listalta `random.from_list`.

## Katso myös

- Gleamin virallinen dokumentaatio: https://gleam.run/
- Satunnaislukujen generointi Pythonilla: https://www.python.org/
- Satunnaislukujen merkitys ohjelmoinnissa: https://medium.com/