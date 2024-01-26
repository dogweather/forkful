---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:50:07.450103-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Mitä ja miksi? Satunnaislukujen generointi tarkoittaa ennustamattomien numeroiden tuottamista. Tätä tapahtuu kun tarvitaan elementtiä sattumanvaraisuudesta – peleissä, simulaatioissa ja tietoturvan alueilla.

## How to:
Koodiesimerkit ja tulosteet:

```Ruby
# Perussatunnaisluku väliltä 0 - 10
puts rand(11)
```

```Ruby
# Satunnainen desimaalinumero
puts rand
```

```Ruby
# Satunnaisluku väliltä 1 - 50
puts rand(1..50)
```

```Ruby
# Satunnaislukujen sarja (tässä: viisi numeroa väliltä 1-100)
Array.new(5) { rand(1..100) }
```

Esimerkkituloste sarjalle viisi satunnaista numeroa:
```
[23, 67, 35, 89, 2]
```

## Deep Dive:
Syväsukellus: 
Ruby käyttää pseudosatunnaislukugeneraattoria (PRNG), joka saa aikaan numeroiden sarjan, joka näyttää satunnaiselta, mutta on toistettavissa. Historiallisesti tietokoneet ovat käyttäneet erilaisia algoritmeja, kuten Mersenne Twisteria, satunnaislukujen luontiin. Vaihtoehtoisia tapoja saada satunnaislukuja on esimerkiksi ottamalla huomioon ulkoisia tekijöitä kuten hiiren liikettä tai näppäimistön napautuksia. Pelialoilla ja salausteoriassa käytetään usein oikeita satunnaislukugeneraattoreita, jotka hyödyntävät fyysistä ilmiötä, kuten kvanttimekaniikan satunnaisuutta.

## See Also:
Lisätietoa ja lähteitä:
- Ruby-dokumentaatio satunnaisluokkaan: [Ruby Random Class](https://ruby-doc.org/core-3.1.0/Random.html)
- Satunnaisuuden filosofia ja sovellukset ohjelmistokehityksessä: [Randomness in Computing](https://en.wikipedia.org/wiki/Randomness)
- Tarkempaa tietoa Mersenne Twister -algoritmista ja sen käytöstä: [Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)
