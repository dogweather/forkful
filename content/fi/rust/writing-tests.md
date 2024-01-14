---
title:                "Rust: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Rust on suosittu ohjelmointikieli sen nopeuden ja turvallisuuden vuoksi. Mutta miten voimme varmistaa, että kirjoitettu koodi toimii odotetusti? Tässä tulevat avuksi testit.

## Miten

Testien kirjoittaminen Rustissa on yksinkertaista ja suoraviivaista. Voit luoda uuden moduulin nimeltä "tests" ja kirjoittaa erilaisia testitapauksia käyttämällä sisäänrakennettua makroa "assert_eq!". Tämä varmistaa, että kunkin testin odotettu tulos on sama kuin todellinen tulos.

```Rust
#[cfg(test)]
mod tests {
    #[test]
    fn test_sum() {
        assert_eq!(2 + 2, 4); // Tämä testi läpäisee odotetusti
        assert_eq!(2 + 2, 5); // Tämä testi ei läpäise, sillä odotettu tulos ja todellinen tulos eivät ole samat
    }
}
```

Voit myös käyttää erilaisia muita makroja, kuten "assert_ne!" ja "assert!", testien kirjoittamiseen eri tilanteisiin. Voit myös luoda omia testi-struktuureja ja käyttää erilaisia asetteluja testien järjestämiseen.

## Syvempi sukellus

Testien kirjoittaminen Rustissa on tärkeä osa koodin kehittämistä ja varmistaa, että ohjelma toimii odotetusti erilaisissa tilanteissa. On tärkeää testata myös virheiden hallintaa ja reunatapauksia. Voit myös suorittaa testit automaattisesti käyttämällä komentoa "cargo test", joka löytää ja suorittaa kaikki testitiedostot projektissasi.

On myös hyvä käytäntö kirjoittaa testejä ennen varsinaisen koodin kehittämistä. Tämä auttaa sinua selventämään odotettuja tuloksia ja varmistaa, että koodi toimii oikein.

## Katso myös

- [Rustin testauksen viralliset dokumentit](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Blogini: Miksi testien kirjoittaminen on tärkeää ohjelmistokehityksessä](https://www.blogi123.com/miksi-testien-kirjoittaminen-on-tarkeaa-ohjelmistokehityksessa)
- [Videotutoriaali: Kuinka kirjoittaa testejä Rustissa](https://www.youtube.com/watch?v=dQw4w9WgXcQ)