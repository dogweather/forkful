---
title:                "Gleam: Tiedonpätkien eristäminen"
simple_title:         "Tiedonpätkien eristäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringien erotuksen tekeminen on tärkeää, kun halutaan pilkkoa merkkijonoja pienempiin osiin ja käsitellä niitä erikseen. Tämä voi olla hyödyllistä esimerkiksi tietokannassa tai käyttäjän syöttämien tietojen käsittelyssä. Gleamin substring-toiminto tarjoaa tehokkaan ja helpon tavan saavuttaa tämä tavoite.

## Kuinka tehdä

Gleam tarjoaa kätevän ```substring```-funktion, joka ottaa parametreiksi halutun merkkijonon, aloitusindeksin ja lopetusindeksin. Esimerkiksi jos halutaan erotella sana "Hei" lauseesta "Hei, mitä kuuluu?", voitaisiin käyttää seuraavaa koodia:

```Gleam
sana = "Hei, mitä kuuluu?"
substring(sana, 0, 3)
```

Tämä tulostaisi ```"Hei"```. Voit myös käyttää negatiivisia indeksejä, jolloin nollaa vastaava indeksi kuvailee merkkijonon loppua. Esimerkiksi:

```Gleam
sana = "Tämä on testi"
substring(sana, -5, -1)
```

Tulostaisi ```"test"```. Näillä muutoksilla voit räätälöidä tulevan substringisi juuri haluamasi pituiseksi.

## Syväsukellus

Gleamilla on myös muita hyödyllisiä substring-operaattoreita, kuten esimerkiksi ```contains```, joka tarkistaa esiintyykö annettu merkkijono toisessa annetussa merkkijonossa, tai jopa ```split```, joka pilkkoo merkkijonon halutun kohdan perusteella. Näitä ja muita Gleamin tarjoamia toimintoja kannattaa tutkia lisää, sillä ne voivat olla erittäin hyödyllisiä substringien käsittelyssä.

## Katso myös

- [Gleam ohjelmointikieli](https://gleam.run/)
- [Gleam dokumentaatio](https://gleam.run/documentation/)
- [Gleam tekstinkäsittelytoiminnot](https://gleam.run/documentation/stdlib/string/)