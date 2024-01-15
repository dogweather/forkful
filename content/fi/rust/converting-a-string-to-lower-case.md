---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "Rust: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi
Um tehdä web-sovelluksia, joissa käyttäjien syöttämät merkkijonot täytyy yhtenäistää ja helpommin vertailla, on hyödyllistä osata muuntaa merkkijono pienaakkosiksi.
## Miten
```Rust
fn main() {
    let s = String::from("TÄMÄ ON ESIMERKKI");
    println!("{}", s.to_lowercase()); //prints "tämä on esimerkki"
}
```
```Rust
//Voidaan myös käyttää merkkijonomuuttujan metodia
fn main() {
    let mut s = String::from("TÄMÄ ON TOINEN ESIMERKKI");
    s.make_ascii_lowercase(); //muuttaa "s" muuttujan arvon "tämä on toinen esimerkki"
    println!("{}", s);
}
```
## Syvempi sukellus
Merkkijonon muuttaminen pienaakkosiksi pohjautuu Unicode-standardin käyttämään Case Maps -algoritmiin. Tässä algoritmissa määriteltään, millaisia muutoksia tulee tehdä merkkijonon eri merkkeihin muuttaakseen ne toivottuun kirjainmuotoon. Tämän ansiosta merkkijonon muuntaminen pienaakkosiksi toimii kaikenlaisilla kielillä ja kirjoitusjärjestelmillä.   
## Katso myös
- [Rustin virallinen dokumentaatio merkkijonon muuntamisesta pienaakkosiksi](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)
- [Ruuttu - paikallinen Rust-yhteisö Suomessa](https://ruuttu.dev/)
- [Rust Hacks - suomennettu blogiartikkeli Rust-harrastajille](https://www.rusthacks.com/)