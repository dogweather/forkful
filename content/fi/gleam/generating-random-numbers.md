---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:49:08.680754-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Arvaa mitä? Softassa tarvitaan usein random-lukuja. Ehkä pukkaat peliä, jossa on oltava ylläreitä, tai sitten hämärrät dataa. Pointti on, että arvaamattomuus on joskus tarpeen.

## How to: (Näin teet:)
```gleam
import gleam/io
import gleam/erlang/atom
import gleam/erlang/time
import gleam/random

fn main() {
  // Alusta seed nykyhetkestä
  let seed = time.now_int()
  let random_gen = random.from_seed(seed)

  // Heitä noppi
  let (roll, _random_gen) = random.int(1..7, random_gen)
  
  // Tulosta nopan heitto
  io.println(atom.to_string(roll))
}

// Esimerkkitulo:
// "4"
```
Kopioi koodi. Aja. Boom! Saat eri numeron joka kerta (tai siis melkein).

## Deep Dive (Sukellus syvälle)
Ennen vanhaan, ts. ennen tietokoneita, numeroiden arvontaa varten piti heilauttaa noppaa tai vetää lapusta hattuun. 50-luvulla digivehkeet ottivat ohjat, mutta tää homma on vähän niin kuin arpapeliä – ei täysin arvaamatonta, kun softa on hari.

Gleamissa käytetään BEAMin, eli Erlangin virtuaalikoneen, perintöä random-luvuille. Voit kylvää siemenen (seed) ja puskea sarjan numeroita. Siemenen asetus ajasta (nykyhetki) on näppärä, koska se muuttuu jatkuvasti. Jos koodit saman siemenen, tulee samat luvut – kätevää testaamiseen.

Tokihan muitakin keinoja on. Tarjolla on eri kirjastoja ja algoritmeja, jotka lupaavat vieläkin parempaa satunnaisuutta. Käytännössä valintaan vaikuttaa tilanne: mitä olet tekemässä, kuinka "randomia" tarvitaan ja tarvitaanko turvallisuutta.

## See Also (Katso myös)
- Erlangin OTP-dokkari, sukulaissielu Gleamille, kertoo numeroarvonnoista: [https://erlang.org/doc/man/rand.html](https://erlang.org/doc/man/rand.html)
- Tietoturva ja kryptograafinen satunnaisuus - keskustelua syvemmistä vesistä, jos tarvitsen sitä turvallisuutta: [https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator](https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator)
