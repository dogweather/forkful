---
title:                "Rust: Satunnaisten lukujen luominen"
programming_language: "Rust"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit tuottaa satunnaisia numeroita? Satunnaiset numerot ovat usein tarpeen monissa ohjelmointitehtävissä, kuten tietokonepelin kehittämisessä tai kryptografian sovelluksissa.

## Miten

Tässä esimerkissä käytämme Rustin standardikirjaston `rand`-kirjastoa satunnaisen luvun tuottamiseen:

```Rust
use rand::Rng;

// Tuottaa satunnaisen luvun väliltä 1-10
let number = rand::thread_rng().gen_range(1, 11);

println!("Satunnainen luku: {}", number);
```

Esimerkkilähtö tulostaa jotain tällaista:

```
Satunnainen luku: 7
```

## Syväluotaus

Satunnaislukuja voidaan tuottaa useilla eri tavoilla, mutta yleensä ne perustuvat johonkin laskennalliseen algoritmiin. Esimerkiksi yksinkertainen tapa tuottaa satunnainen luku on käyttämällä lineaarista siirtomuunnosta (linear congruential generator). Tämä algoritmi ottaa seedin, joka on lähtökohtaisesti alkuperäinen arvo, ja käyttää sitä tuottamaan uusia lukuja. Tärkeintä on, että algoritmi on ennustettavissa ja tuottaa vain pseudosatunnaisia lukuja.

On tärkeää huomata, että mikään tietokone ei pysty tuottamaan täysin satunnaisia lukuja, koska ne perustuvat aina johonkin algoritmiin. Siksi on tärkeää valita algoritmi, joka on tarpeeksi monimutkainen ja jolla on hyvät satunnaisuudelle tärkeät ominaisuudet, kuten seedin riippuvuus ja huonojen lukuja välttäminen.

## Katso myös
- [Rustin dokumentaatio satunnaisista numeroista](https://doc.rust-lang.org/std/rand/index.html)
- [Algoritmit pseudosatunnaisuuden luomiseen](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [Satunnaisuuden rooli tietotekniikassa](https://plato.stanford.edu/entries/chance-randomness/)