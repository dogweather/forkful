---
title:                "Sattumanvaraisten numeroiden generointi"
html_title:           "Rust: Sattumanvaraisten numeroiden generointi"
simple_title:         "Sattumanvaraisten numeroiden generointi"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Satunnaisten numeroiden luominen on tärkeä osa ohjelmointia, ja sitä tekevät kaikki ohjelmoijat. Se tulee joko tarpeesta luoda jokin satunnainen elementti tiettyyn tarkoitukseen tai vain lisätä mielenkiintoa peliin tai sovellukseen. Ohjelmoijat käyttävät satunnaisia numeroita esimerkiksi arpajaisissa, satunnaisten tapahtumien arvioinnissa ja kaikenlaisten pelien ja simulaatioiden luomisessa.

## Miten:
Rust tarjoaa valtavan määrän tapoja luoda satunnaisia numeroita, ja tässä on muutama esimerkki:

```Rust 
// Luodaan yksi satunnainen numero 1 ja 100 välille
let random_num = rand::random::<u32>() % 100 + 1;

// Luodaan satunnainen luku viiden ja kymmenen välille
let rand_num = rand::thread_rng().gen_range(5,11);

// Luodaan satunnainen boolean-arvo
let random_bool = rand::random::<bool>();

// Luodaan satunnainen kirjain merkkijonosta
let random_char = b"abcdefghijklmnopqrstuvwxyz"[rand::thread_rng().gen_range(0, 26)] as char;
```

Nämä ovat vain muutamia esimerkkejä, mutta Rustilla on runsaasti muita vaihtoehtoja satunnaisten numeroiden luomiseen.

## Syvä sukellus:
Historiallisesti satunnaisia numeroita on luotu muun muassa laskemalla konenäytöiden kohinaa ja käyttämällä satunnaisia fyysisiä tapahtumia, kuten kolikon heittämistä. Nykyään tällaisia menetelmiä käytetään harvemmin, koska ne ovat haavoittuvaisia ja teknologian kehitys on tarjonnut parempia ratkaisuja.

Rust tarjoaa monia kirjastoja satunnaislukujen generoimiseen, kuten "rand" ja "rand_core". Näiden kirjastojen avulla ohjelmoijat voivat luoda satunnaisia numeroita eri muodoissa ja tarpeisiin.

## Katso myös:
- [Rustin virallinen dokumentaatio kirjastoista](https://doc.rust-lang.org/stable/rust-by-example/std_misc/rand.html)
- [Rustin rand-dokumentaatio](https://rust-random.github.io/book/)
- [Rustin turvallisuus ja satunnaislukujen generointi](https://www.reddit.com/r/rust/comments/bxrwgy/are_your_random_numbers_truly_random_and_how_to/)