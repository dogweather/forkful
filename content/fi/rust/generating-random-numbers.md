---
title:                "Rust: **Tasauslukujen tuottaminen"
simple_title:         "**Tasauslukujen tuottaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluat luoda satunnaisia numeroita ohjelmoinnissa? Monet tehtävät ja sovellukset vaativat satunnaisia numeroita, kuten pelit, simulaatiot ja tekoälyalgoritmit. Rust-ohjelmointikielen sisäänrakennettu Random-moduuli tarjoaa luotettavan tavan luoda satunnaisia numeroita ohjelmissasi.

## Kuinka Tehdä

Rust-ohjelmointikielen sisäänrakennettu Random-moduuli tarjoaa useita erilaisia tapoja luoda satunnaislukuja. Tässä on muutama esimerkki:

```Rust
use rand::{thread_rng, Rng};

// Luo satunnainen kokonaisluku välillä 1-10
let mut rng = thread_rng();
let random_number = rng.gen_range(1, 11);
println!("Satunnainen numero 1-10 välillä on: {}", random_number);

// Luo satunnainen liukuluku välillä 0.0-1.0
let random_float: f64 = rng.gen();
println!("Satunnainen liukuluku 0.0-1.0 välillä on: {}", random_float);

// Luo satunnainen boolean-arvo
let random_bool = rng.gen_bool(0.5);
println!("Satunnainen boolean-arvo on: {}", random_bool);
```

Esimerkissä käytetään Random-moduulin `gen_range()`-funktiota luomaan satunnainen kokonaisluku valitulta väliltä. `gen()`-funktio puolestaan luo satunnaisen liukuluvun väliltä 0.0-1.0 ja `gen_bool()`-funktio luo satunnaisen boolean-arvon, jossa on 50% todennäköisyys saada `true` tai `false`.

## Syventävä Tarkastelu

Random-moduulissa on myös muita hyödyllisiä funktioita, kuten `choose()` ja `shuffle()`. `choose()`-funktio valitsee satunnaisen elementin annetusta listasta ja `shuffle()`-funktio sekoittaa listan elementtejä satunnaisessa järjestyksessä. Tämä voi olla erittäin hyödyllistä esimerkiksi korttipelien tai listojen satunnaiseen järjestämiseen. Tässä on esimerkki käyttö:

```Rust
use rand::{thread_rng, seq::SliceRandom};

// Luo lista numeroista 1-10
let numbers = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

// Valitse satunnainen numero listalta
let mut rng = thread_rng();
let random_number = numbers.choose(&mut rng).unwrap();
println!("Satunnainen numero listalta on: {}", random_number);

// Sekoita listan numerot satunnaisessa järjestyksessä
let mut shuffled_numbers = numbers.clone();
shuffled_numbers.shuffle(&mut rng);
println!("Listan numerot sekoitettuna: {:?}", shuffled_numbers);
```

Tässä esimerkissä käytetään ThreadRng-lukugeneraattoria ja SliceRandom-työkaluja. Kuten näet, Rustin Random-moduuli tarjoaa monia erilaisia tapoja luoda satunnaisia arvoja riippuen tarpeistasi.

## Katso Myös

- Rustin virallinen dokumentaatio Random-moduulille: https://doc.rust-lang.org/std/rand/
- Esimerkkejä käyttökohteista ja lisätietoja Random-moduulista: https://chrismorgan.info/blog/rng-rundown/
- Rustin oppiminen: https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/