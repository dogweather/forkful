---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:35.986100-07:00
description: "Testien kirjoittaminen Rustissa k\xE4sitt\xE4\xE4 automaattisten tarkistusten\
  \ luomisen varmistamaan, ett\xE4 koodisi toimii odotetulla tavalla. Ohjelmoijat\
  \ tekev\xE4t\u2026"
lastmod: '2024-03-13T22:44:56.360155-06:00'
model: gpt-4-0125-preview
summary: "Testien kirjoittaminen Rustissa k\xE4sitt\xE4\xE4 automaattisten tarkistusten\
  \ luomisen varmistamaan, ett\xE4 koodisi toimii odotetulla tavalla."
title: Testien kirjoittaminen
weight: 36
---

## Miten:
Rustin sisäänrakennettu testikehys tukee yksikkö-, integraatio- ja dokumentaatiotestejä ilman ulkopuolisten kirjastojen tarvetta. Testit merkitään `#[test]`-annotaatiolla, ja mikä tahansa näin merkitty funktio kääntyy testiksi.

### Yksikkötestin kirjoittaminen:
Sijoita yksikkötestit testaamaansa moduuliin käyttäen `tests`-alimoduulia, joka on merkitty `#[cfg(test)]`-annotaatiolla varmistaaksesi, että ne kääntyvät vain testattaessa.

```rust
// lib.rs tai main.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(add(2, 2), 4);
    }
}
```

Testien suorittaminen:
```shell
$ cargo test
```

Tuloste:
```shell
   Kompiloidaan your_package_name v0.1.0 (/polku/sinun_pakettiisi)
    Valmis testi [optimointi + debuginfo] kohteille in 0.00 sek
     Suoritetaan yksikkötestejä src/lib.rs (tai src/main.rs)

suoritetaan 1 testi
test tests::it_adds_two ... ok

testitulos: ok. 1 läpäisi; 0 epäonnistui; 0 ohitettiin; 0 mitattiin; 0 suodatettiin pois
```

### Integraatiotestien kirjoittaminen:
Integraatiotestit sijoitetaan `tests`-hakemistoon projektisi ylimmälle tasolle, `src`:n viereen. Jokainen `.rs`-tiedosto `tests`-hakemistossa kääntyy omaksi erilliseksi cratesikseen.

```rust
// tests/integration_test.rs
use your_package_name;

#[test]
fn it_adds_two() {
    assert_eq!(your_package_name::add(2, 2), 4);
}
```

### Testaaminen suosittujen kolmannen osapuolen kirjastojen kanssa:
Laajempien testausominaisuuksien saavuttamiseksi `proptest`-kirjasto voi generoida laajan valikoiman syötteitä funktioiden testaamiseen.

Lisää `proptest` kehitysriippuvuudeksi `Cargo.toml`-tiedostoon:

```toml
[dev-dependencies]
proptest = "1.0"
```

Käytä `proptest`ia suorittamaan sama testi monilla automaattisesti generoiduilla syötteillä:

```rust
// sisällä tests/integration_test.rs tai moduulin #[cfg(test)]
 
use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        your_package_name::add(a, b);
    }
}
```

Tämä tarkistaa, ettei `add` panikoi laajalla valikoimalla `i32` syötteitä.
