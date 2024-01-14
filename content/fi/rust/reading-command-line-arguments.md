---
title:                "Rust: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi?

Monet ohjelmat tarvitsevat syöttötietoja, ja komentoriviparametrit ovat yksi tapa saada nämä tiedot ohjelmaan. Rustissa on hyvä tapa lukea komentoriviparametreja, joka on helppo oppia ja mahdollistaa monenlaisten syöttötietojen käsittelyn.

## Kuinka?
 
Lue komentoriviparametrit kätevällä `clap`-kirjastolla:

```Rust
extern crate clap;  // Lisätään riippuvuus

use clap::Arg;  // Otetaan käyttöön Arg-tyyppi

fn main() {
    let matches = clap::App::new("Ohjelman nimi")
                          .version("1.0")
                          .author("Tekijän nimi <tekijan@email.com>")
                          .about("Ohjelmakuvaus")
                          .arg(Arg::with_name("parametri")
                               .short("p")
                               .long("parametri")
                               .takes_value(true)
                               .help("Parametrin tarkempi kuvaus"))
                          .get_matches();

    if let Some(parametri) = matches.value_of("parametri") {
        println!("Annettu parametri: {}", parametri);
    }
}
```

Kun ajamme ohjelman komentorivillä esimerkiksi `ohjelma -p testi`, tulostuu `Annettu parametri: testi`.

## Syvempi sukellus

Komentoriviparametrien lukeminen ei välttämättä ole tarpeen jokaiselle ohjelmalle, mutta se on hyödyllinen taito hallita Rust-ohjelmoinnissa. `clap`-kirjasto tarjoaa paljon muitakin mahdollisuuksia, kuten parametrien pakollisuuden määrittämisen ja erilaisten arvojen sallimisen. Lisäksi voit lukea komentoriviparametrit eri tavoin, kuten `ArgMatches`-rakenteen avulla.

## Katso myös

- [clap-kirjaston dokumentaatio](https://docs.rs/clap/)
- [Rustin opas komentoriviparametreille](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
- [Komentoriviparametrien käyttöön sovellettava esimerkkikoodi](https://github.com/clap-rs/clap/blob/master/examples/)