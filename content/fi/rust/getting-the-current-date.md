---
title:                "Rust: Tämänhetkisen päivämäärän saaminen"
simple_title:         "Tämänhetkisen päivämäärän saaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi
Tervetuloa lukemaan tämän blogikirjoituksen Rust-ohjelmoinnista ja siitä, miten saat nykyisen päivämäärän. Rust on moniparadigmainen ja nopeasti kasvava ohjelmointikieli, joka on saanut suosiota ympäri maailmaa. Nykyinen päivämäärä on tärkeä osa monia ohjelmia ja Rustissa sen saaminen voi olla hieman erilaista muihin ohjelmointikieliin verrattuna.

## Miten
Alla olevassa koodiesimerkissä näytämme, miten voit käyttää Rustia saadaksesi nykyisen päivämäärän. (Huomaa, että tämän koodin toimimiseen vaaditaan Rustin standardikirjasto.)

```Rust
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

let now = SystemTime::now();
let seconds = now.duration_since(UNIX_EPOCH).unwrap().as_secs();
println!("Nykyinen aika UNIX-aikana: {}", seconds);

```

Kun suoritat tämän koodin, saat ulostulona nykyisen päivämäärän UNIX-aikana, joka on tammikuun 1. 1970 klo 00:00:00 UTC:sta kulunut aika.

## Syventävä tieto
Rustilla on monia erilaisia tapoja saada nykyinen päivämäärä, mutta yllä oleva koodiesimerkki käyttää SystemTime-tyyppiä, joka antaa lisäksi tietoa siitä, kuinka kauan ohjelman suorittaminen on kestänyt. Voit myös käyttää toista tyyppiä, Local, joka palauttaa nykyisen päivämäärän paikallisessa aikavyöhykkeessä. Voit lukea lisää näistä Rustin dokumentaatiosta.

## Katso myös
-https://www.rust-lang.org/
-https://doc.rust-lang.org/std/time/struct.SystemTime.html
-https://doc.rust-lang.org/std/time/struct.Local.html