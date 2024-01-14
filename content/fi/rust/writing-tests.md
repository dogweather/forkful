---
title:    "Rust: Testien kirjoittaminen"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Miksi: Miksi Rust-ohjelmointikielen testien kirjoittaminen on tärkeää?

Testien kirjoittaminen on tärkeä osa ohjelmistokehitysprosessia, sillä se auttaa varmistamaan koodin toimivuuden ja vähentämään mahdollisten virheiden määrää. Rustin vahva tyypitys ja muistin hallinta tekevät siitä erittäin luotettavan ohjelmointikielen, mutta hyvin kirjoitetut testit voivat auttaa löytämään ja korjaamaan mahdollisia bugeja vielä ennen ohjelman julkaisua.

## Kuinka: Esimerkkejä testien kirjoittamisesta Rustissa

Testien kirjoittaminen Rustissa on suhteellisen helppoa, sillä kieli tarjoaa integroidun testikehyksen. Alla on esimerkki yksinkertaisesta testistä, joka tarkistaa, että funktio laskee kahden luvun summan oikein:

```Rust
// Tuodaan tarvittavat moduulit
use std::io;

// Määritellään yksinkertainen funktio summan laskemiseen
fn laske_summa(a: i32, b: i32) -> i32 {
    return a + b;
}

// Testataan funktiota
#[test]
fn testaa_summa() {
    assert_eq!(laske_summa(2, 2), 4);
    assert_eq!(laske_summa(5, 7), 12);
}
```

Testin suorittaminen antaa seuraavanlaisen tulosraportin:

```bash
running 1 test
test testaa_summa ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

Voit myös lisätä testiin erilaisia inputteja ja odotettuja outputteja, jotta varmistat, että funktio toimii erilaisilla syötteillä.

## Syvällisempi sukellus: Lisätietoa testien kirjoittamisesta Rustissa

Rustin testikehys tarjoaa monia hyödyllisiä toimintoja, kuten `assert_eq!`, joka vertaa kahta arvoa ja ilmoittaa testin epäonnistuneeksi, jos ne eivät ole samat. Voit myös käyttää `assert_ne!`-funktiota, joka tarkistaa, että kaksi arvoa eivät ole samat.

Rustissa voit myös luoda erityisiä testitiedostoja, jotka suoritetaan vain tiettyjen komentoriviparametrien avulla. Tämä voi olla hyödyllistä esimerkiksi erilaisten integraatiotestien suorittamiseen ennen ohjelman julkaisua.

Voit lukea lisää Rustin testikehyksestä ja sen tarjoamista mahdollisuuksista [Rustin virallisesta dokumentaatiosta](https://doc.rust-lang.org/book/ch11-01-writing-tests.html).

## Katso myös

- [Rustin viralliset testaamisohjeet](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [Rustin testaaminen VS Code -ympäristössä](https://www.forrestthewoods.com/blog/how-to-run-rust-tests-in-visual-studio-code/)
- [Amazing Rust: Writing Effective Tests in Rust](https://amazingrust.com/posts/2021-03-31-writing-effective-tests/)