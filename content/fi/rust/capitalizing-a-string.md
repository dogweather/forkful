---
title:                "Merkkijonon kirjainten muuttaminen isoiksi"
html_title:           "Rust: Merkkijonon kirjainten muuttaminen isoiksi"
simple_title:         "Merkkijonon kirjainten muuttaminen isoiksi"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Rust Ohjelmointi: Miten suurella alkukirjaimella alkaa merkkijono

## Mikä & Miksi?

Suurella alkukirjaimella alkavia merkkijonoja käytetään ohjelmoinnissa normisto- ja navigointitarkoituksiin. Se tarkoittaa jokaisen sanan muuttamista niin, että ensimmäinen kirjain on iso ja muut kirjaimet pieniä.

## Miten tehdä:

Rust-ohjelmointikielessä emme voi suoraan muuttaa merkkijonon ensimmäisen kirjaimen suureksi, koska Rustin merkkijonot (Strings) ovat immuuttisia. Seuraavassa esimerkissä näytämme, kuinka tämä voidaan saavuttaa:

```Rust
fn paaoma(mut s: String) -> String {
    if let Some(r) = s.get_mut(0..1) {
        r.make_ascii_uppercase();
    }
    s
}

fn main() {
    let s = String::from("rust ohjelmointi");
    println!("{}", paaoma(s));
}
```

Yllä oleva koodi antaa seuraavan tulosteen:

```
Rust ohjelmointi
```

## Sukellus

Historiallisesti katsottuna eri ohjelmointikielet ovat tarjonneet erilaisia tapoja toteuttaa merkkijonot suurella alkukirjaimella. Esimerkiksi JavaScriptissä on `toUpperCase`-metodi ja Javassa `toUpperCase()`. Rustissa on monia tapoja tehdä tämä, joista yksi on yllä olevassa esimerkissä käytetty.

Rustissa on kuitenkin olemassa kirjastojen, kuten `titlecase` tai `propper`, jotka tarjoavat toiminnallisuuden merkkijonon muuntamiseen suurella alkukirjaimella.

Rustissa merkkijonot ovat oletusarvoisesti immuuttisia. Tämä immuuttisuus on tehty muistiturvallisuuden paranttaminen mielenkiinnossa.

## Katso myös:

Lisää merkkijonojen käsittelemisestä Rustissa löytyy osoitteesta: [link](https://doc.rust-lang.org/book/ch08-02-strings.html)

Lisätietoja kirjastosta `titlecase`: [link](https://crates.io/crates/titlecase)

Lisätietoja kirjastosta `propper`: [link](https://crates.io/crates/propper)