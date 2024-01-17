---
title:                "Tilapäistiedoston luominen"
html_title:           "Rust: Tilapäistiedoston luominen"
simple_title:         "Tilapäistiedoston luominen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Luodessa ohjelmia, saatat törmätä tarpeeseen luoda väliaikainen tiedosto. Tämä tarkoittaa, että tilapäinen tiedosto luodaan väliaikaisesti ja se poistetaan automaattisesti, kun se ei enää ole tarpeellinen. Ohjelmoijat tekevät tämän esimerkiksi varastoidakseen väliaikaisia tietoja tai suorittaakseen väliaikaisia tehtäviä.

## Kuinka tehdä se:
Rustilla on sisäänrakennettu `tempfile` kirjasto, joka helpottaa väliaikaisten tiedostojen luomista. Sieltä löytyy `NamedTempFile`-rakenne, jota voidaan käyttää tilapäisen tiedoston luomiseen annetulla nimellä. Alla on esimerkki koodi, joka luo tilapäisen tiedoston nimeltään "example.txt" ja kirjoittaa siihen "Tämä on väliaikainen tiedosto." Vielä lopuksi se tulostaa tiedoston sisällön konsoliin.
```Rust
use std::io::prelude::*;
use std::fs::File;
use tempfile::NamedTempFile;

let mut temp_file = NamedTempFile::new().expect("Tiedoston luomisessa tapahtui virhe");
writeln!(temp_file, "Tämä on väliaikainen tiedosto.").expect("Kirjoittamisessa tapahtui virhe");
println!("Tiedoston sisältö: {}", temp_file.path().display());
```
Tämän esimerkin tulostus olisi:
```
Tiedoston sisältö: /tmp/example.txt
```

On myös mahdollista luoda tilapäisiä tiedostoja eri muodossa, kuten `TempDir`-rakenteen avulla, joka luo väliaikaisen hakemiston.

## Syvemmälle:
Luotaessa tilapäisiä tiedostoja, on tärkeää muistaa poistaa ne, kun ne eivät enää ole tarpeellisia, jotta ohjelmasi ei täyttyisi tarpeettomilla tiedostoilla. `tempfile` kirjasto hoitaa tämän ihanteellisesti ja varmistaa, että tilapäiset tiedostot poistetaan myös, jos ohjelmasi kaatuu ennen niiden poistamista.

On myös muita vaihtoehtoja tilapäisten tiedostojen luomiseen, kuten `fs::File::create` toiminto, mutta näissä tapauksissa sinun täytyy manuaalisesti varmistaa tiedoston poistaminen.

Tilapäisten tiedostojen käyttö on yleinen käytäntö ohjelmoijien keskuudessa, ja Rustin `tempfile` kirjasto tekee sen helpoksi ja turvalliseksi.

## Katso myös:
Lisätietoa Rustin `tempfile` kirjastosta löytyy sen virallisesta dokumentaatiosta: https://docs.rs/tempfile/3.1.0/tempfile/.

Voit myös lukea lisää tilapäisten tiedostojen käytöstä ja turvallisesta tiedostonhallinnasta Rustissa täältä: https://doc.rust-lang.org/book/ch12-03-improving-error-handling-and-modularity.html#managing-resources-by-implementing-drop.