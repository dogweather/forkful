---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testaus on koodin automaattista tarkastelua varmistamaan sen toimivuus odotetulla tavalla. Testit tunnistavat virheet aikaisin ja auttavat ylläpitämään koodin laatua projektin kasvaessa.

## How to:
Rustissa moduulitestiä kirjoitetaan `#[cfg(test)]` -attribuutilla ja `#[test]`-annotaatiolla. Alla on esimerkki yksinkertaisesta testistä.

```Rust
#[cfg(test)]
mod tests {
    #[test]
    fn se_toimii() {
        assert_eq!(2 + 2, 4);
    }
}
```

Aja testit käskyllä: `cargo test`

Esimerkkituloste:

```
running 1 test
test tests::se_toimii ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

## Deep Dive
Testaus on tärkeä osa ohjelmistokehitystä. Rust alkoi tukea moduulitestejä varhaisessa vaiheessa, inspiroituneena muista kielistä, kuten Ruby'n RSpec:stä. Vaihtoehtoina ovat integraatiotestit ja ulkopuolisten testiframeworkejen, kuten Criterion, käyttö suorituskykytestaukseen. Rust testaa oletuksena rinnakkaistetusti, mutta rinnakkaistusta voi säätää tai kytkeä pois päälle.

## See Also
Rustin virallinen testausdokumentaatio: [The Rust Programming Language - Tests](https://doc.rust-lang.org/book/ch11-00-testing.html)
Crates.io testausframeworkit: [Crates.io - Testing](https://crates.io/categories/development-tools::testing)
