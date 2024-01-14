---
title:    "Rust: Tiedoston lukeminen"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Miksi lukea tekstitiedosto Rust-ohjelmoijille?

Tekstitiedostot ovat yleinen tapa tallentaa tietoja erilaisiin ohjelmistoihin ja projekteihin. Lukemalla tekstitiedostoja, voit helposti lukea ja käsitellä tietoja ohjelmissasi. Tämä Rust-ohjelman avulla voit oppia, miten voit lukea tekstitiedostoja ja käyttää niiden sisältöä omassa koodissasi.

## Miten lukea tekstitiedosto Rustilla

Käytä seuraavia vaiheita lukeaksesi tekstitiedosto Rustilla:

1. Avaa tiedosto käyttäen `File::open()` -funktiota, joka ottaa parametrina tiedoston polun.
2. Käytä `read_to_string()` -funktiota, joka lukee tiedoston sisällön ja palauttaa sen String-muodossa.
3. Käytä `unwrap()` -funktiota koodissa varmistaaksesi, että tiedoston lukeminen onnistuu.
4. Voit nyt käyttää String-muuttujaa ja sen sisältöä ohjelmassasi!

```Rust
use std::fs::File;

let tiedosto = File::open("tiedosto.txt");
let sisalto = tiedosto.read_to_string().unwrap();
println!("Tiedoston sisältö: {}", sisalto);
```

## Syvempi sukellus tekstitiedoston lukemiseen

Vaikka yllä oleva esimerkki toimii hyvin yksinkertaisissa tapauksissa, se ei ole välttämättä tehokkain tapa lukea tekstitiedostoja Rustilla. Voit myös käyttää `BufReader` -tyyppiä ja `lines()` -funktiota lukeaksesi tiedoston rivi kerrallaan.

```Rust
use std::fs::File;
use std::io::{BufReader, BufRead};

let tiedosto = File::open("tiedosto.txt").expect("Tiedoston avaaminen epäonnistui!");
let lukija = BufReader::new(tiedosto);

for rivi in lukija.lines() {
    println!("{}", rivi.unwrap());
}
```

Tämä lähestymistapa on tehokkaampi suurten tiedostojen lukemiseen, koska se ei lataa koko tiedostoa kerralla muistiin, vaan lukee sen rivi kerrallaan.

# Katso myös

- [Rustin vakio-ohjelmointikirjaston dokumentaatio](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Rust By Example: Tiedoston lukeminen](https://doc.rust-lang.org/rust-by-example/std_misc/file/read_lines.html)
- [BcnRust: Tekstitiedoston käsittely Rustilla](https://ferrous-systems.com/blog/rust-text-processing/)