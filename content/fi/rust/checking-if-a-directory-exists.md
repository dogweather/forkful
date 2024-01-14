---
title:    "Rust: Tarkistetaan, onko hakemisto olemassa"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Miksi?

On monia syitä, miksi haluat tarkistaa, onko tietty hakemisto olemassa Rust-ohjelmassa. Ehkä haluat varmistaa, että ohjelma ei yritä käsitellä tiedostoja, jotka eivät ole käytettävissä. Tai ehkä haluatte luoda uuden hakemiston, jos sitä ei jo ole olemassa. Joka tapauksessa, hakemistojen olemassaolon tarkistaminen on tärkeä osa turvallista ja tehokasta ohjelmointia Rustissa.

## Miten tehdä?

Rustilla on sisäänrakennettu "fs" kirjasto, joka tarjoaa työkaluja tiedostojen ja hakemistojen käsittelyyn. Tarkistaaksesi, onko hakemisto olemassa, voit käyttää "path::is_dir" -funktiota. Tämä funktio palauttaa totuusarvon, jos kyseinen polku johtaa hakemistoon. Voit myös käyttää "path::metadata" -funktiota, joka palauttaa tiedostomuotoilun tiedot ja voit tarkistaa, onko se hakemisto.

```Rust
use std::fs;

fn main() {
    let path = "hakemisto/";
    if fs::metadata(path).unwrap().is_dir() {
        println!("Hakemisto on olemassa!");
    } else {
        println!("Hakemistoa ei löydy.");
    }
}
```

```
Hakemisto on olemassa!
```

## Syvällinen sukellus

"fs::metadata" -funktiolla on myös muita hyödyllisiä ominaisuuksia, kuten "created" -tiedon palauttaminen, joka kertoo, milloin tiedosto tai hakemisto on luotu. Voit myös käyttää "path::canonicalize" -funktiota, joka muuntaa annetun polun absoluuttiseksi ja ratkaisee mahdolliset symboliset linkit.

## Katso myös

- [fs::metadata dokumentaatio](https://doc.rust-lang.org/std/fs/fn.metadata.html)
- [fs::canonicalize dokumentaatio](https://doc.rust-lang.org/std/fs/fn.canonicalize.html)