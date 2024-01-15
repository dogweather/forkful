---
title:                "Testien kirjoittaminen"
html_title:           "Rust: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaessa ohjelmia, testien kirjoittaminen voi tuntua ylimääräiseltä työltä ja hidastavan projektin etenemistä. Kuitenkin testien kirjoittamisella voi säästää aikaa ja vaivaa myöhemmin, kun ohjelmaa täytyy muokata tai korjata virheitä. Testien avulla voidaan myös varmistaa, että ohjelman eri osat toimivat oikein ja ennaltaehkäistä mahdollisia virheitä.

## Näin tehdään

Testien kirjoittaminen Rustissa on helppoa ja yksinkertaista. Ensimmäiseksi täytyy lisätä `#[cfg(test)]` merkintä moduulien yhteyteen, jotta testit voidaan suorittaa. Sen jälkeen testit voidaan kirjoittaa `#[test]` ja `assert!()` funktioiden avulla. Alla on esimerkki testin kirjoittamisesta `String` datatypea varten:

```rust
#[cfg(test)]
mod test_string {
    #[test]
    fn test_string_contains() {
        let my_string = String::from("Hello world");
        assert!(my_string.contains("Hello"));
    }
}
```

Jokainen `#[test]` merkinnällä varustettu funktio on yksi testi, ja `assert!()` funktio tarkistaa, että annettu lauseke on tosi. Tässä tapauksessa testi on totta, sillä "Hello world" sisältää sanan "Hello". Nyt voimme ajaa testin käyttämällä `cargo test` komentoa, joka tulostaa seuraavan:

```
running 1 test
test test_string_contains ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 3 filtered out
```

Voit myös käyttää `assert_eq!()` funktiota tarkistaaksesi, että kaksi arvoa ovat yhtäsuuret. Alla on esimerkki siitä, miten voit testata kahden numeron summan:

```rust
#[cfg(test)]
mod test_sum {
    #[test]
    fn test_sum_result() {
        let a = 2;
        let b = 3;
        let result = a + b;
        assert_eq!(result, 5);
    }
}
```

Tässä tapauksessa testi on tosi, sillä 2 ja 3:n summa on 5. Voit nyt taas ajaa testin ja näet, että kaikki testit läpäisevät.

## Syvemmälle aiheeseen

Testien kirjoittaminen Rustissa voi olla erittäin hyödyllistä monilla eri tavoilla. Yksi tärkeimmistä eduista on se, että testien avulla voit olla varma, että kirjoittamasi koodi toimii oikein. Voit myös lisätä testejä jokaiseen muokattuun osaan koodia, jotta voit varmistaa, että uudet muutokset eivät riko toimivaa koodia.

Testien avulla voit myös huomata mahdollisia virheitä ja ongelmakohtia koodissasi. Testien kirjoittaminen pakottaa sinut ajattelemaan ohjelmaasi eri näkökulmista ja löytämään mahdolliset virhetilanteet, joita käyttäjät saattavat kohdata.

Lisäksi testien avulla voit helposti varmistaa, että ohjelmasi toimii oikein eri ympäristöissä ja platvormeilla. Rustin testauskehys tarjoaa mahdollisuuden suorittaa testeihin tarvittavat tapahtumat ja testata useita eri tilanteita.

## Katso myös

- [Rustin testausdokumentaatio](https://doc.rust-lang.org/rust-by-example/testing.html)
- [Testauksen merkitys ohjelmoinnissa (englanniksi)](https://medium.com/@