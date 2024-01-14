---
title:    "Rust: Väliaikaisen tiedoston luominen"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi luoda väliaikainen tiedosto?

Väliaikaiset tiedostot ovat tarpeellisia monissa tilanteissa, kun ohjelmassa tarvitaan väliaikaista tallennustilaa esimerkiksi tiedon käsittelyssä. Niitä voidaan käyttää esimerkiksi uuden tiedoston luomisessa, väliaikaisessa tiedon tallentamisessa tai jopa testaamisessa. Rustin avulla väliaikaisen tiedoston luominen on suoraviivaista ja tehokasta.

## Näin luodaan väliaikainen tiedosto Rustissa

Väliaikaisen tiedoston luomiseksi tarvitsemme `std::fs::File` -moduulin ja `tempfile`-paketin, jotka voidaan lisätä Rust projektiin `Cargo.toml` -tiedostossa. Tämän jälkeen voimme käyttää `tempfile::tempfile()` -funktiota luomaan uuden väliaikaisen tiedoston ja avata sen kirjoitus- ja lukuoikeuksilla:

```Rust
// Lisätään tarvittavat moduulit
use std::fs::File;
use tempfile::tempfile;

// Luodaan väliaikainen tiedosto
let temp_file = tempfile().expect("Väliaikaisen tiedoston luominen epäonnistui");

// Avataan tiedosto kirjoitus- ja lukuoikeuksilla
let mut file = File::tempfile_in(temp_file.path()).expect("Tiedoston avaaminen epäonnistui");
```

Voimme myös määrittää väliaikaisen tiedoston nimen ja polun käyttämällä `tempfile::Builder`:ia:

```Rust
// Luodaan väliaikainen tiedoston polku
let temp_path = tempfile::Builder::new()
               .prefix("my_temp_file")
               .suffix(".txt")
               .tempfile()
               .expect("Väliaikaisen tiedoston luominen epäonnistui")
               .into_temp_path();

// Avataan tiedosto kirjoitus- ja lukuoikeuksilla
let mut file = File::create(&temp_path).expect("Tiedoston avaaminen epäonnistui");
```

## Sydän syvällä – tarkempi tarkastelu väliaikaisen tiedoston luomisesta

Väliaikainen tiedosto luodaan käyttämällä `tempfile::tempfile()` -funktiota, joka palauttaa `Result<NamedTempFile, TempPath>` -tyypin. Tämän jälkeen voimme käyttää `NamedTempFile` -tyypin metodien avulla tiedoston ominaisuuksien muokkaamiseen.

Rustissa väliaikaisten tiedostojen käyttö on turvallista, sillä tiedostot poistetaan automaattisesti, kun `NamedTempFile` -tyyppi on käsitelty loppuun ja se poistetaan muistista. Tämä säästää käyttäjän päänvaivaa manuaalisesta tiedoston poistamisesta.

## Katso myös

- [std::fs::File dokumentaatio](https://doc.rust-lang.org/std/fs/struct.File.html)
- [tempfile crate dokumentaatio](https://docs.rs/tempfile/3.1.0/tempfile/)
- [std::path::Path dokumentaatio](https://doc.rust-lang.org/std/path/struct.Path.html)