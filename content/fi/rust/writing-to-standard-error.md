---
title:                "Kirjoittaminen standardi-virheelle"
html_title:           "Rust: Kirjoittaminen standardi-virheelle"
simple_title:         "Kirjoittaminen standardi-virheelle"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Kirjoittaminen standardivirheeseen ("standard error") on tapa, jolla ohjelmoijat voivat näyttää virheviestejä ja muuta informaatiota terminaalissa. Tämä auttaa ohjelmoijia havaitsemaan ja korjaamaan mahdollisia ongelmia ohjelmassaan.

## Miten:

```Rust
use std::io::Write; //Tarvitaan kirjoittamiseen
use std::io::stderr; //Standardivirhe
writeln!(stderr(), "Tämä on esimerkki virheviestistä."); //Käytetään writeln-makroa merkkijonon kirjoittamiseen standardivirheeseen
```

Esimerkissä käytetään Rustin standardikirjaston io-moduulia kirjoittamiseen ja erityisesti stderr-toimintoa standardivirheen käsittelyyn. Käytettäessä writeln-makroa, voimme tulostaa haluamamme tekstin standardivirheessä.

```Rust
fn main() {
    eprintln!("Toinen esimerkki virheviestistä.");
}
```

Erikoismerkki "e" ennen println-funktiota viittaa siihen, että tulostus tapahtuu standardivirheessä.

## Syvempi katsaus:

Kirjoittaminen standardivirheeseen on tärkeä osa ohjelmien kehittämistä ja virheenkäsittelyä. Ennen standardin käyttöönottoa monissa ohjelmointikielissä, kuten C:ssä, oli yleistä käyttää fprintf-funktiota virheviestien tulostamiseen terminaalin virhevirtaan. Tämä toiminto nykyään vastaa Rustin eprint-makroa.

On myös mahdollista ohjata standardivirheen tulostus tiedostoon tai muuhun laitteeseen, kuten stdout-funktioon. Tämä voidaan tehdä muokkaamalla ohjelmoijan ympäristöä tai antamalla ohjelmalle parametreja.

Kirjoittaminen standardivirheeseen on yleinen tapa ilmoittaa virheistä ja varoituksista ohjelmassa, ja se on osa hyvien ohjelmointikäytäntöjen noudattamista.

## Katso myös:

- [Rust Standard Library](https://doc.rust-lang.org/std/index.html)
- [Rust eprint-makron dokumentaatio](https://doc.rust-lang.org/std/macro.eprint.html)
- [C fprintf-funktion dokumentaatio](https://www.cplusplus.com/reference/cstdio/fprintf/)