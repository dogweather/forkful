---
title:                "Vianmääritystulostuksen tulostaminen"
html_title:           "Rust: Vianmääritystulostuksen tulostaminen"
simple_title:         "Vianmääritystulostuksen tulostaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Rust on tehokas ja turvallinen ohjelmointikieli, joten sitä käytetään usein raskaissa ja turvallisissa sovelluksissa, kuten verkkopalvelimissa ja sulautetuissa järjestelmissä. Debuggaus on tärkeä osa sovellusten kehitystä, jotta voidaan löytää ja korjata mahdollisia virheitä. Siksi on tärkeää ymmärtää, miten tulostaa debug-tietoja Rust-koodissa.

## Miten

Rustissa on useita tapoja tulostaa debug-tietoja. Yksinkertaisin tapa on käyttää `println!` -makroa, joka tulostaa annetut arvot konsoliin. Esimerkiksi:

```Rust
let num = 5;
println!("Luku on: {}", num);
```

Tämä tulostaa konsoliin "Luku on: 5". `println!` -makron lisäksi voit käyttää myös muita makroja, kuten `dbg!`, `eprintln!` ja `format!`, saadaksesi erilaisia tulostusominaisuuksia. Voit myös käyttää muotoilumerkkejä, kuten `{}` ja `{:#?}`, muokataksesti tulostuksen ulkoasua.

## Syvemmälle

Rustin `println!` -makro toimii samalla tavalla kuin C-kielen `printf`-funktio, mutta se on turvallisempi, koska Rust varmistaa, että kaikki annetut arvot vastaavat oikeaa muotoa. Tämä auttaa välttämään muistiongelmia ja muita yleisiä virheitä.

Rustissa voit myös käyttää `Debug` ja `Display` -traitteja mukauttaaksesi tulostusmuotoa tietorakenteille, kuten `struct` ja `enum`. Tämä antaa sinulle enemmän hallintaa siitä, miten haluat näyttää dataa debug-tulosteissa. Voit myös koota useita arvoja yhteen tuplessa ja tulostaa ne kaikki kerralla käyttämällä `dbg!` -makroa.

## Katso myös

- [The Rust Book](https://doc.rust-lang.org/book/)
- [Rust Debugging Guide](https://docs.rs/crate/debugging-guide/0.1.2)
- [Rust Pro Tips](https://careers.rust-lang.org/t/rust-pro-tips/139)