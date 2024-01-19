---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tulostaminen debug-tulostus on ohjelmointitekniikka, jossa tulostetaan ohjelman tilatietoja, esimerkiksi muuttujien arvoja, ohjelman suorittamisen aikana. Sitä käytetään auttamaan ohjelmoijia ymmärtämään ja korjaamaan ohjelmien virheitä.

## Näin teet:

Rustissa debug-tulostaminen on yksinkertaista ja intuitiivistä. Tässä on perusesimerkki:

```Rust
let x = 5;
println!("x:n arvo on: {:?}", x);
```
Tämä tulostaa: `x:n arvo on: 5`

Debug-tulostamiseen voidaan käyttää myös rakenteita ja tietoliikennemuotoja:

```Rust
#[derive(Debug)]
struct Rakenne {
    nimi: String,
    arvo: i32,
}

let y = Rakenne {
    nimi: String::from("Y"),
    arvo: 10,
};

println!("{:?}", y);
```

Tämä tulostaa: `Rakenne { nimi: "Y", arvo: 10 }`

## Syvemmälle:
Debug-tulostaminen on ollut ohjelmoinnin perustekniikka jo vuosikymmenien ajan. Sitä käytetään yleisesti kaikissa ohjelmissa, kaikilla ohjelmointikielillä.

Rustin lähdekoodin tarkistukseen ja debug-tulostamiseen on useita vaihtoehtoja. Esimerkiksi, voit käyttää `Debug`-merkintää tai `Display`-merkintää. Kuitenkin `Display` ei sovi debug-tulostamiseen, koska se vaatii erityisen tulostusformaatin.

Rustin `println!` makro käyttää alla olevaa Rustin formaattikirjastoa toteuttamaan debug-tulostamisen. Tämä tekee debug-tulostamisesta erittäin joustavaa ja tehokasta.

## Katso myös:

- Rust-by-Example debuggaus: https://doc.rust-lang.org/stable/rust-by-example/hello/print/print_display.html
- Debug-makron dokumentaatio: https://doc.rust-lang.org/std/fmt/struct.Debug.html