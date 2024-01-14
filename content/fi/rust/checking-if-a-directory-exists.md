---
title:    "Rust: Tarkistaako hakemisto on olemassa"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmassasi voi olla tarve tarkistaa, onko tietty hakemisto olemassa. Tämä voi olla tarpeen esimerkiksi, kun haluat varmistaa, että tietty hakemisto on olemassa ennen kuin lisäät siihen tiedostoja tai suoritat muita toimintoja. Onneksi Rust-kielessä on helppo tapa tarkistaa hakemiston olemassaolo.

## Miten tehdä se

Rustilla on käytössä `std::fs` kirjasto, joka tarjoaa pääsyn tiedostojärjestelmään liittyviin toimintoihin. Tarkistaaksesi, onko hakemisto olemassa, voit käyttää `std::fs::metadata` -funktiota antamalla sille hakemiston polun parametrina. Funktio palauttaa `Result<Metadata>` -tyypin arvon, joka kertoo hakemiston tiedoista, kuten sen olemassaolosta.

```Rust
use std::fs;

let directory_path = "polku/hakemistoon";

if let metadata = fs::metadata(directory_path) {
    println!("Hakemisto on olemassa!");
} else {
    println!("Hakemistoa ei ole olemassa");
}
```

Tässä esimerkissä käytetään `if let` -lauseketta, joka suorittaa koodin, jos `fs::metadata` palauttaa onnistuneen arvon. Voit myös käyttää `match` -lauseketta, jos haluat käsitellä sekä onnistuneen että virheilmoituksen tapaukset.

## Syvällisempi sukellus

`std::fs::metadata` -funktion avulla voit myös saada tarkempia tietoja hakemistosta. Funktio palauttaa `Metadata` -tyypin rakenteen, joka sisältää esimerkiksi hakemiston luomisajan ja muokkausajan. Voit käyttää näitä tietoja esimerkiksi tiedostonjärjestelmissä, joissa tarvitaan tarkempaa käsittelyä hakemistojen suhteen.

## Katso myös

- [Rustin `std::fs` kirjasto](https://doc.rust-lang.org/std/fs/index.html)
- [Rustin `Result` tyyppi](https://doc.rust-lang.org/std/result/index.html)
- [Rustin tiedostojärjestelmän käsittely](https://doc.rust-lang.org/book/ch12-00-an-io-project.html)