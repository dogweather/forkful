---
title:    "Gleam: Satunnaislukujen generoiminen."
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi osallistua satunnaislukujen generointiin?

Satunnaislukujen generointi on olennainen osa monia ohjelmointitehtäviä. Se voi olla tarpeellista simuloinneissa, pelien kehittämisessä tai jopa tietoturvassa. Gleam-ohjelmointikieli tarjoaa helpon tavan generoida satunnaislukuja ja tässä blogikirjoituksessa kerromme tarkemmin miten se tapahtuu.

## Kuinka tehdä se: Gleam-ohjelmointiesimerkkejä

Gleam tarjoaa useita sisäänrakennettuja moduuleja satunnaislukujen generointiin. Yksi kätevä tapa tehdä se on käyttää `Random` -moduulia ja sen `int` -funktiota. Katsotaan esimerkiksi seuraavaa koodinpätkää, joka tulostaa satunnaisen luvun väliltä 1-100:

```
Gleam import Random

let random_number = Random.int(1, 100)

Gleam.io.format("Random number: {}", [random_number])
```

Tämän koodin tuloste voisi näyttää vaikkapa tältä:

```
Random number: 42
```

Voit myös määrittää oman satunnaislukusi seedin, jolloin saat aina saman satunnaisluvun. Tämä voi olla hyödyllistä esimerkiksi testauksessa. Katso seuraavaa esimerkkiä:

```
Gleam import Random

let random_number = Random.int(1, 100, seed: 123)
Gleam.io.format("Random number with seed: {}", [random_number])

let same_random_number = Random.int(1, 100, seed: 123)
Gleam.io.format("Same random number with seed: {}", [same_random_number])
```

Tämän koodin tuloste näyttää siltä:

```
Random number with seed: 74
Same random number with seed: 74
```

Tässä näet kuinka `Random` -moduulin avulla voit helposti generoida satunnaisia lukuja.

## Syväsukellus: Lisätietoa satunnaislukujen generoinnista

Satunnaislukujen generointiin liittyy monia matemaattisia algoritmeja ja näiden ymmärtäminen voi auttaa sinua luomaan tarkempia ja monipuolisempia satunnaislukugeneraattoreita. Gleam-documentation tarjoaa lisätietoa käytetyistä algoritmeista ja niiden toiminnasta, joten suosittelemme tarkastamaan sen syvempää ymmärrystä varten.

## Katso myös

- Dokumentaatio satunnaislukujen generoinnista Gleam-ohjelmointikielellä: https://gleam.run/books/standard-library/Random.html
- Lisätietoa Gleam-ohjelmointikielestä: https://gleam.run/
- Valmiita satunnaislukuesimerkkejä ja -tehtäviä: https://www.educative.io/blog/code-random-number-generator