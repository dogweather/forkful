---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "Rust: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Luodessaan väliaikaisia tiedostoja, ohjelmoijat voivat hallita tiedostojärjestelmän resursseja tehokkaasti ja välttää turhia pysyvien tiedostojen luomisia. Tämä vähentää muistin käyttöä ja pitää järjestelmän puhtaampana.

## Kuinka

Rustilla on sisäänrakennettu tiedostojen hallintatoiminto, joka mahdollistaa luotujen tiedostojen väliaikaisen tallentamisen ja automaattisen poistamisen käytön jälkeen. Tämä tapahtuu käyttämällä `tempfile` -kirjastoa, joka sisältää funktion `TempFile::create()`, joka luo väliaikaisen tiedoston ja palauttaa `std::io::Result` vakiosuorituksen.

```Rust
use std::fs::File;
use tempfile::TempFile;

let mut temporary_file = TempFile::create().unwrap();
temporary_file.write_all(b"Hello world!").unwrap();

// Tarkasta luodun tiedoston olemassaolo
assert!(temporary_file.as_file().is_ok());
```

Väliaikaisen tiedoston poistaminen tapahtuu automaattisesti, kun siihen liittyvät objektit tuhoutuvat. Tämä tapahtuu tyypillisesti kun `temporary_file`-muuttuja poistuu käytöstä.

## Syvä sukellus

Väliaikaisen tiedoston luominen voi olla hyödyllistä esimerkiksi tiedon tallentamisessa tietokantoihin tai web-sovellusten välimuistitallennukseen. On myös hyvä huomata, että `tempfile`-kirjasto tarjoaa myös muita hyödyllisiä ominaisuuksia kuten väliaikaisen hakemiston luomisen `tempdir`-funktiolla.

## Katso myös

- [Official Rust Documentation - std::fs::File](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Tempfile Documentation](https://docs.rs/tempfile/3.1.0/tempfile/)