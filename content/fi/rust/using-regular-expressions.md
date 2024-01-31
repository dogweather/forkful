---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-01-19
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions, eli säännölliset lausekkeet, ovat mallipohjaisia merkkijonoja tekstin tunnistukseen ja käsittelyyn. Ohjelmoijat käyttävät niitä monimutkaisen tekstidataan etsimiseen, validoimiseen ja modifiointiin nopeasti ja tehokkaasti.

## How to:
Asennetaan `regex`-kirjasto lisäämällä `Cargo.toml`-tiedostoon riippuvuus:

```toml
[dependencies]
regex = "1.5.4"
```

Peruskäyttö esimerkki hakemaan päivämääriä:

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\d{4}-\d{2}-\d{2}").unwrap();
    let text = "Tärkeitä päivämääriä: 2023-04-01, 2022-12-24.";

    for date in re.find_iter(text) {
        println!("Löydetty päivämäärä: {}", date.as_str());
    }
}
```

Tuloste:

```
Löydetty päivämäärä: 2023-04-01
Löydetty päivämäärä: 2022-12-24
```

## Deep Dive
Regular expressions eli regex, juontaa juurensa 1950-luvulta, matemaatikko Stephen Kleenen teoriasta. Alternatiiveina toimii tekstinkäsittelymenetelmät kuten Stringin omat metodit ja parserit, muta regex on yleisesti tehokkain. Rustin `regex`-kirjaston suorituskyky on huippuluokkaa, ja se käyttää automaattisesti "lazy DFA" algoritmia tehon maksimointiin.

## See Also
- Rust `regex`-kirjasto: https://crates.io/crates/regex
- Regex101, säännöllisten lausekkeiden testaamista varten: https://regex101.com/
- Rust-koodausohjeet ja style-guide: https://doc.rust-lang.org/cargo/
