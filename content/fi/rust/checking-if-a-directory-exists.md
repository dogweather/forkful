---
title:                "Rust: Tarkista onko hakemisto olemassa"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi tarkistaa hakemisto löytyy
On monia syitä, miksi haluat tarkistaa, onko tietty hakemisto olemassa Rust-ohjelmassasi. Esimerkiksi voit haluta varmistaa, että hakemisto on luotu ennen tiedostojen tallentamista sinne tai määrittää ohjelman käyttäytymisen, jos hakemisto puuttuu. Onneksi Rust tarjoaa helpon tavan tarkistaa, onko hakemisto olemassa.

## Miten tehdä se
Rustissa voit käyttää `PathBuf`-luokkaa tarkistaaksesi, onko hakemisto olemassa. Ensinnäkin varmista, että tuot `std::path::PathBuf` luokan käyttöön:

```rust
use std::path::PathBuf;
```

Sitten voit luoda uuden `PathBuf` -olion antamalla sille polun:

```rust
let polku = PathBuf::from("polku/hakemistoon");
```

Nyt kun meillä on `PathBuf` -olio, voimme käyttää `exists` -metodia tarkistaaksemme, onko hakemisto olemassa:

```rust
if polku.exists() {
    println!("Hakemisto on olemassa!");
} else {
    println!("Hakemistoa ei löytynyt.");
}
```

## Syvällinen käsittely
Vaikka `exists` -metodi onkin helppo käyttää, se ei välttämättä toimi odotetulla tavalla. Jos hakemisto ei ole luotu, mutta sillä on sama nimi kuin olemassa olevalla tiedostolla, `exists` -metodi palauttaa `true` -arvon, joka voi aiheuttaa ongelmia. Tämän välttämiseksi voit käyttää `is_dir` -metodia, joka palauttaa `true` vain, jos käsiteltävä polku on hakemisto.

```rust
if path.is_dir() {
    println!("Hakemisto on olemassa!");
} else {
    println!("Hakemistoa ei löytynyt.");
}
```

Voit myös käyttää `is_file` -metodia varmistaaksesi, että polku on tiedosto eikä esimerkiksi tunnelukon tiedosto.

## Katso myös
- [Rustin virallinen dokumentaatio PathBuf-luokasta](https://doc.rust-lang.org/std/path/struct.PathBuf.html)
- [Rustin virallinen dokumentaatio Path-luokasta](https://doc.rust-lang.org/std/path/struct.Path.html)
- [Rustin opetusohjelma hakemistojen hallinnasta](https://doc.rust-lang.org/book/ch12-03-improving-error-handling-and-modularity.html)