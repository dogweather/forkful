---
title:                "Gleam: Satunnaislukujen luominen"
programming_language: "Gleam"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä miksi voi olla hyödyllistä generoida satunnaisia numeroita ohjelmointityössä. Voit käyttää niitä esimerkiksi testaamiseen, pelien luomiseen tai salausalgoritmien kehittämiseen.

## Kuinka

Gleam-lohkareiden avulla on helppoa ja nopeaa generoida satunnaisia numeroita. Alla on esimerkkejä koodista ja tulosteista.

```Gleam
let random_number = Random.int(1, 10) // Generoi satunnaisen kokonaisluvun väliltä 1-10
```

```Gleam
let random_float = Random.float(0.0, 1.0) // Generoi satunnaisen liukuluvun väliltä 0.0-1.0
```

```Gleam
let random_bool = Random.bool() // Generoi satunnaisen totuusarvon 
```

```Gleam
let random_string = Random.string(8) // Generoi satunnaisen merkkijonon, jonka pituus on 8 merkkiä
```

Tässä vain muutama esimerkki, mutta Gleam tarjoaa myös muita tapoja generoida satunnaisia lukuja erilaisilla parametreilla.

## Syvempi sukellus

Gleam käyttää Mersenne Twister -algoritmia satunnaisten lukujen generoimiseen. Tämä algoritmi on yleisesti käytössä monissa ohjelmointikielissä ja se on todettu luotettavaksi ja nopeaksi.

## Katso myös

- [Gleamin virallinen dokumentaatio](https://gleam.run/documentation)
- [Mersenne Twister -algoritmi](https://en.wikipedia.org/wiki/Mersenne_Twister)