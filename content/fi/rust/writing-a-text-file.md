---
title:    "Rust: Tiedostotiedoston kirjoittaminen"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Haluatko oppia kirjoittamaan tekstitiedostoja Rust-ohjelmointikielellä? Tässä blogikirjoituksessa näytämme sinulle, kuinka se tehdään!

## Miten tehdä

Seuraavassa esimerkissä näytämme yksinkertaisen tavan kirjoittaa tekstitiedosto Rust-ohjelmointikielellä:

```rust
use std::io::prelude::*;
use std::fs::File;
 
fn main() {
    let data = "Tämä on tekstiä, joka kirjoitetaan tiedostoon käyttäen Rustia.";
    let mut tiedosto = File::create("output.txt")
        .expect("Ei voitu luoda tiedostoa!");
    tiedosto.write_all(data.as_bytes())
        .expect("Ei voitu kirjoittaa tiedostoon!");
}
```

Tässä koodissa käytämme `std::io` ja `std::fs`-kirjastoja kirjoittaaksemme tiedostoon. Ensiksi määritämme `data`-muuttujan, joka sisältää tekstiä. Sitten luomme tiedoston nimeltä `output.txt` käyttäen `File::create`-funktiota. Lopuksi kirjoitamme `data`-muuttujan sisällön tiedostoon käyttämällä `write_all`-metodia.

Suorittamalla tämän ohjelman, voit huomata, että se luo uuden `output.txt`-nimisen tiedoston ja kirjoittaa sinne `data`-muuttujan sisällön.

## Syvempää tietoa

Kuten huomaat, Rust tarjoaa kätevät työkalut tekstitiedostojen kirjoittamiseen. Voit myös käyttää `write`-metodia, joka kirjoittaa yhden tavun kerrallaan tiedostoon. Lisäksi voit käyttää `flush`-metodia varmistaaksesi, että kaikki tiedoston sisältö on kirjoitettu ennen kuin ohjelma päättyy.

## Katso myös

- [Rustin virallinen oppimissivusto](https://www.rust-lang.org/learn)
- [Rust-kielen dokumentaatio](https://doc.rust-lang.org/stable/book/)