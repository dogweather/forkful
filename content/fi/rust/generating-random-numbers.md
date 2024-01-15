---
title:                "Satunnaislukujen luominen"
html_title:           "Rust: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet kiinnostunut ohjelmoinnista ja haluat oppia uuden kielen, Rust on ehdottomasti kokeilemisen arvoinen vaihtoehto. Se tarjoaa tehokkaan ja käyttäjäystävällisen ratkaisun moniin ohjelmointiongelmiin. Esimerkiksi, koodissa tarvitaan usein satunnaislukuja, kuten arpajaisohjelmassa tai pelissä petokseen. Tässä artikkelissa opit, miten voit generoida satunnaisia lukuja Rustissa.

## Kuinka

Rust tarjoaa vaivattoman tavan generoida satunnaisia lukuja käyttäen `rand` kirjastoa. Ensiksi tarvitset tämän kirjaston asentamista projektiisi. Voit tehdä tämän lisäämällä `rand = "0.8.3"` kirjaston riippuvuuksiin `Cargo.toml` tiedostossasi. 

Seuraavaksi, lisää seuraava koodi `main.rs` tiedostoon:

``` Rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    // Generoi satunnainen numero väliltä 1-10
    let random_num: u8 = rng.gen_range(1, 11);
    println!("Satunnainen numero: {}", random_num);
}
```

Kääntämällä ja suorittamalla koodin, saat tällaisen tulosteen:

```
Satunnainen numero: 7
```

Koodissa käytetään `Rng` traitia ja `thread_rng` metodia luomaan satunnaislukugeneraattori. Sitten `gen_range` metodia käytetään luomaan satunnainen luku halutulta väliltä, tässä tapauksessa 1-10.

## Syvempi sukellus

Rustissa on myös muita tapoja generoida satunnaisia lukuja, kuten `rng.gen()` joka generoi satunnaisen luvun koko tarjottujen tyypin alueelta. Tämä mahdollistaa esimerkiksi satunnaisen numeron generoinnin myös merkkijonotyypille.

``` Rust
// Generoi satunnainen merkki ASCII aakkosten joukosta
let random_char: char = rng.gen();
println!("Satunnainen kirjain: {}", random_char);
```

Lisäksi, voit myös asettaa alkuperäisen siemenarvon satunnaisen luvun generoinnille käyttämällä `rng.seed()` metodia. Tämä takaa, että jokainen kerta kun ohjelma suoritetaan, satunnaiset luvut ovat erilaisia.

## Katso myös

- [Rustin virallinen dokumentaatio](https://doc.rust-lang.org/std/rand/)
- [Rustin `rand` kirjaston Github-sivut](https://github.com/rust-random/rand)