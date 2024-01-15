---
title:                "Sattumanvaraisten lukujen luominen"
html_title:           "Gleam: Sattumanvaraisten lukujen luominen"
simple_title:         "Sattumanvaraisten lukujen luominen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi
Miksi haluaisit käyttää satunnaislukugenerointia ohjelmoinnissa? Syitä voi olla monia, mutta yleisimpiä ovat pelit, tilastolliset tutkimukset ja salauksen generointi.

Satunnaislukujen generointi on tärkeä työkalu monissa ohjelmointiprojekteissa. Se voi lisätä pelin dynamiikkaa, antaa tärkeitä tilastollisia tietoja tai varmistaa turvallisuutta kryptografisissa sovelluksissa.

## Miten
Gleamissa on useita tapoja generoida satunnaislukuja. Yksinkertaisin tapa on käyttää Gleamin sisäänrakennettua `random` -kirjastoa, joka tarjoaa erilaisia funktioita satunnaislukujen luomiseen.

```
Gleam import random

let number = random.generate_int(1,10) // Generoi satunnaisen kokonaisluvun väliltä 1-10
```

Voit myös luoda satunnaisen merkkijonon käyttämällä `random.string` -funktiota:

```
Gleam import random

let string = random.generate_string(10) // Generoi 10 merkin mittaisen satunnaisen merkkijonon
```

Kun generoit satunnaisia lukuja, on tärkeää huolehtia niiden tasaisesta jakautumisesta sekä suuresta riippumattomuudesta. Gleamin `random` -kirjasto takaa tämän varmistamalla korkealaatuiset satunnaislukugeneraattorit.

## Syvempi sukellus
Satunnaislukujen generointi on mielenkiintoinen matemaattinen ilmiö ja sillä on tärkeä rooli monissa tieteen ja teknologian osa-alueissa.

Yksi tärkeimmistä asioista satunnaislukujen generoinnissa on sen epädeterministisyys eli seuraavan luvun ennustamattomuus. Gleamin `random` -kirjasto käyttää tämän varmistamiseen kryptografisia algoritmeja, jotka antavat suuren määrän todennäköisiä arvoja.

Toinen mielenkiintoinen osa satunnaislukujen generointia on sen käyttötilanteiden moninaisuus. Monien ohjelmointitehtävien ratkaiseminen vaatii satunnaisuuden käyttöä ja Gleam tarjoaa helpon ja luotettavan tavan tämän saavuttamiseen.

## Katso myös
- Gleamin virallinen dokumentaatio satunnaislukugeneroinnista: [https://gleam.run/lib/random](https://gleam.run/lib/random)
- Satunnaislukujen käyttö pelimoottorin ohjelmoinnissa: [https://gafferongames.com/post/fixyourstep/](https://gafferongames.com/post/fixyourstep/)
- Satunnaislukujen käyttö salauksessa: [https://www.cloudflare.com/learning/ssl/how-randomness-works-in-tls/](https://www.cloudflare.com/learning/ssl/how-randomness-works-in-tls/)