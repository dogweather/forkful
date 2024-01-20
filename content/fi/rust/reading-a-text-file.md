---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tekstitiedoston lukeminen on prosessi, jossa ohjelma ottaa syötteenä tekstitiedoston ja tulkitsee sen sisällön. Ohjelmoijat tekevät tämän tiedon hallinnan, datan analysoinnin ja tiedostojen käsittelyn mahdollistamiseksi.

## Kuinka:

Seuraavassa on yksinkertainen Rust-koodinäyte tekstitiedoston lukemiseen.

```Rust
use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn lue_tiedosto(tiedoston_nimi: &str) -> io::Result<()> {
    let tiedosto = File::open(tiedoston_nimi)?;
    let lukija = BufReader::new(tiedosto);

    for rivi in lukija.lines() {
        println!("{}", rivi?);
    }

    Ok(())
}
```

Tämä koodi palauttaa seuraavanlaisen tulosteen:

```Rust
Hello, world!
This is a text file.
```

## Syväsukellus:

1. Historiallinen konteksti: Tekstitiedostojen lukeminen on perusta useissa ohjelmointikielissä. Rust on otettu käyttöön tarjoamaan turvallisempi ja tehokkaampi tapa tehdä samat tehtävät.

2. Vaihtoehdot: On olemassa useita muita metodeja tekstitiedostojen lukemiseen Rustissa, kuten "fs::read_to_string":n ja "fs::read":n käyttäminen.

3. Toteutus yksityiskohdat: Rustin "File::open" avaa tiedoston ja "BufReader::new" luo uuden puskuroivan lukijan. "for" silmukka käy läpi lukijan rivit ja tulostaa ne.

## Katso myös:

- Rust by Example, luku [File I/O](https://doc.rust-lang.org/rust-by-example/std_misc/file/open.html): On erinomaisia rust-esimerkkejä ja oppaita käytettävissäsi.

- Rust's [Read Trait documentation](https://doc.rust-lang.org/std/io/trait.Read.html): Tämän linkin takaa löydät Rustin virallisen lukuominaisuuden dokumentaation.

- [The Rust Programming Language Book, Chapter 9.5](https://doc.rust-lang.org/stable/book/ch09-02-recoverable-errors-with-result.html#shortcuts-for-panic-on-error-the-unwrap-and-expect-calls): Tämä luku käsittelee yksityiskohtaisemmin virheiden käsittelyä Rustissa.