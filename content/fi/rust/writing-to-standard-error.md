---
title:    "Rust: Kirjoittaminen standardivirheeseen"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Miksi kirjoittaa virherajoitin

Monet ohjelmointikielet tarjoavat mahdollisuuden tulostaa virheitä standardivyörajaan, joka on teksti, joka näkyy ohjelman lokitiedostossa tai komentorivin tulosteessa. Tämä on erittäin hyödyllinen työkalu ohjelmointivirheiden tunnistamiseen ja korjaamiseen.

## Kuinka tehdä se

Rust-ohjelmointikieli tarjoaa helpon ja tehokkaan tavan kirjoittaa virheilmoituksia standardiulostuloon. Tämä voidaan tehdä käyttämällä `eprintln!` -makroa, joka toimii samalla tavalla kuin `println!` mutta tulostaa virheitä standardivirheeseen.

Esimerkki:

```Rust
use std::io;

fn main() {
    let input = io::stdin().read_line().unwrap();
    if input.parse::<i32>().is_err() {
        eprintln!("Virheellinen syöte. Syötä kokonaisluku.");
    } else {
        println!("Syöte oli oikein.");
    }
}
```

Tämä koodi lukee käyttäjän syötettä ja tarkistaa, onko se kokonaisluku. Jos sitä ei voida muuttaa kokonaisluvuksi, virheilmoitus tulostetaan standardivirheeseen. Muussa tapauksessa tulostetaan ilmoitus, joka osoittaa, että syöte oli oikein.

Tuloste:

```
$ cargo run
123abc
Virheellinen syöte. Syötä kokonaisluku.
```

## Syvällinen tarkastelu

Rustin `eprintln!`-makro on enemmän kuin vain ominaisuus mukava virheilmoitusten tulostamiseen. Se myös tarjoaa mahdollisuuden lisätä dynaamisia arvoja virheilmoituksiin, mikä tekee virheilmoituksista hyödyllisempiä korjata. Esimerkiksi, jos haluamme näyttää tarkemman syyn virheeseen, voimme käyttää `eprintln!`-makron kanssa muuttujia:

```Rust
use std::io;

fn main() {
    let input = io::stdin().read_line().unwrap();
    if input.parse::<i32>().is_err() {
        eprintln!("Virheellinen syöte {}. Syötä kokonaisluku.", input.trim());
    } else {
        println!("Syöte oli oikein.");
    }
}
```

Tuloste:

```
$ cargo run
123abc
Virheellinen syöte 123abc. Syötä kokonaisluku.
```

Käyttämällä `eprintln!`-makron kanssa muuttujia, voimme paremmin paikantaa ja korjata virheitä.

## Katso myös

- [Rust-kielen virallinen dokumentaatio](https://doc.rust-lang.org/std/macro.eprintln.html)
- [Virherajoittimen käyttö C++:ssa](https://stackoverflow.com/questions/13944258/how-to-print-to-stderr-in-c)
- [Vinkkejä ohjelmointivirheiden hallitsemiseen](https://www.pluralsight.com/guides/how-to-handle-errors-in-your-rust-application)