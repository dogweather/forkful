---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Lua: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Hakemiston olemassaolon tarkistaminen tarkoittaa sitä, että ohjelmoija tarkistaa koodissaan, onko tietty hakemisto jo luotu järjestelmään. Tätä tehdään yleensä välttääksemme virheitä, kuten tiedoston kirjoittamista hakemistoon, jota ei ole olemassa.

## Kuinka:

Tarkistetaan onko hakemisto olemassa Rust-ohjelmointikielessä näin:

```Rust
use std::path::Path;

fn main() {
    if Path::new("/polku/hakemistoon").exists() {
        println!("Hakemisto on olemassa");
    } else {
        println!("Hakemistoa ei ole olemassa");
    }
}
```

Tämän ohjelman tulostus olisi joko "Hakemisto on olemassa" tai "Hakemistoa ei ole olemassa", riippuen siitä onko annettu hakemisto jo luotu.

## Syvällistä tietoa:

Hakemiston olemassaolon tarkistamiseen on ollut tarve jo kauan, ja se on hyvin yleinen tehtävä monilla ohjelmointikielillä. Rustissa `Path::new().exists()` on kätevin tapa tarkistaa se. On myös muita menetelmiä, kuten `fs::metadata()` funktion käyttö, mutta ne vaativat enemmän koodia ja ovat monimutkaisempia. `Path::new().exists()` toimii suoraan `std::path::Path` rakenteeseen, joka on osa Rustin standardikirjastoa ja se tarkistaa sekä tiedostojen että hakemistojen olemassaolon.

## Katso myös:

Rustin virallinen dokumentaatio antaa hyvää tietoa aiheesta:
- Rust-syntaksin ja standardikirjaston perusteet: https://doc.rust-lang.org/book/
- Tarkistetaan tiedoston tai hakemiston olemassaolo: https://doc.rust-lang.org/std/path/struct.Path.html#method.exists