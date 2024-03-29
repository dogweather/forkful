---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:36.675655-07:00
description: "Virheilmoitusten kirjoittaminen standardivirheeseen (stderr) Rustissa\
  \ tarkoittaa virheilmoitusten ja diagnostiikkatietojen ohjaamista konsoliin erill\xE4\
  \xE4n\u2026"
lastmod: '2024-03-13T22:44:56.373673-06:00'
model: gpt-4-0125-preview
summary: "Virheilmoitusten kirjoittaminen standardivirheeseen (stderr) Rustissa tarkoittaa\
  \ virheilmoitusten ja diagnostiikkatietojen ohjaamista konsoliin erill\xE4\xE4n\u2026"
title: Kirjoittaminen standardivirheeseen
---

{{< edit_this_page >}}

## Mikä & Miksi?
Virheilmoitusten kirjoittaminen standardivirheeseen (stderr) Rustissa tarkoittaa virheilmoitusten ja diagnostiikkatietojen ohjaamista konsoliin erillään standarditulosteesta (stdout). Ohjelmoijat tekevät näin erottaakseen ohjelman normaalin tulosteen virheilmoituksista, mikä helpottaa virheiden asianmukaista käsittelyä tai niiden ohjaamista lokiin tai tiedostoihin suorituksen aikana.

## Kuinka:
Rust tarjoaa suoraviivaisen tavan kirjoittaa stderr:iin käyttämällä `eprintln!`-makroa, samankaltaisesti kuin `println!` käytetään stdoutille. Tässä on perusesimerkki:

```rust
fn main() {
    eprintln!("Tämä on virheilmoitus!");
}
```

Esimerkkituloste (standardivirheeseen):
```
Tämä on virheilmoitus!
```

Jos haluat enemmän kontrollia virheilmoitusten yli, kuten tekstiä muotoillessasi tai I/O-tuloksia käsitellessäsi, käytä `stderr` funktiota `std::io` moduulista. Tämä menetelmä tarjoaa kahvan globaaliin stderr-virtaan, johon voit sitten kirjoittaa käyttäen esimerkiksi `write_all` tai `writeln` metodeja `Write` piirteestä:

```rust
use std::io::{self, Write};

fn main() {
    let stderr = io::stderr();
    let mut kahva = stderr.lock();
    
    writeln!(kahva, "Muotoiltu virheilmoitus: {}", 404).expect("Kirjoittaminen stderr:iin epäonnistui");
}
```

Esimerkkituloste (standardivirheeseen):
```
Muotoiltu virheilmoitus: 404
```

Jos työskentelet ympäristöissä tai sovelluksissa, joissa tukeudut kirjastoihin lokitukseen tai virheenkäsittelyyn, `log` ja `env_logger` kirjastot ovat suosittuja. Vaikka niitä käytetään enemmän lokitustarkoituksiin, ne ovat mukautettavissa ja voivat ohjata virhelokitustasot stderr:iin. Alla on yksinkertainen käyttöesimerkki `log` ja `env_logger` kanssa:

Lisää ensin riippuvuudet `Cargo.toml`-tiedostoosi:
```toml
[dependencies]
log = "0.4"
env_logger = "0.9"
```

Sitten, aseta ja käytä loki sovelluksessasi:
```rust
fn main() {
    env_logger::init();
    log::error!("Tämä on virheilmoitus, joka on kirjattu stderr:iin");
}
```

Tämän ohjelman suorittaminen (asetettuasi `env_logger` asianmukaisella ympäristömuuttujalla, esimerkiksi `RUST_LOG=error`) tulostaa virheilmoituksen stderr:iin, hyödyntäen lokitusinfrastruktuuria.

```plaintext
ERROR: Tämä on virheilmoitus, joka on kirjattu stderr:iin
```
