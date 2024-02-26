---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:33.208382-07:00
description: "Merkkijonon ensimm\xE4isen kirjaimen muuttaminen suuraakkoseksi Rustissa\
  \ tarkoittaa merkkijonon muokkaamista siten, ett\xE4 sen ensimm\xE4inen merkki muutetaan\u2026"
lastmod: '2024-02-25T18:49:53.265924-07:00'
model: gpt-4-0125-preview
summary: "Merkkijonon ensimm\xE4isen kirjaimen muuttaminen suuraakkoseksi Rustissa\
  \ tarkoittaa merkkijonon muokkaamista siten, ett\xE4 sen ensimm\xE4inen merkki muutetaan\u2026"
title: Merkkijonon muuttaminen isoiksi kirjaimiksi
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonon ensimmäisen kirjaimen muuttaminen suuraakkoseksi Rustissa tarkoittaa merkkijonon muokkaamista siten, että sen ensimmäinen merkki muutetaan suuraakkoseksi, jos se on kirjain, samalla kun loput merkkijonosta jätetään muuttumattomiksi. Ohjelmoijat suorittavat usein tämän toimenpiteen muotoilutarkoituksissa, kuten valmistellessaan sanoja otsikoita varten tai varmistaakseen johdonmukaisuuden käyttäjän syötteessä.

## Kuinka:

Merkkijonon ensimmäisen kirjaimen muuttamiseksi suuraakkoseksi Rustissa on kaksi pääreittiä: standardikirjaston toiminnallisuuksien käyttö tai kolmannen osapuolen pakettien käyttö monimutkaisempien tai erityistarpeiden kannalta. Tässä on, miten voit tehdä molemmat.

### Käyttäen Rustin Standardikirjastoa

Rustin standardikirjasto ei tarjoa suoraa menetelmää merkkijonojen suuraakkostamiseksi, mutta voit saavuttaa tämän manipuloimalla merkkijonon merkkejä.

```rust
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let my_string = "hello";
    println!("{}", capitalize_first(my_string)); // Tuloste: Hello
}
```

### Käyttäen `heck`-pakettia

Suoraviivaisempaa lähestymistapaa varten, erityisesti työskennellessä laajemmassa tekstinkäsittelykontekstissa, saatat mieluummin käyttää kolmannen osapuolen kirjastoja, kuten `heck`. `heck`-paketti tarjoaa erilaisia kirjainkoon muuntotoiminnallisuuksia, mukaan lukien yksinkertaisen tavan muuttaa merkkijonojen ensimmäinen kirjain suuraakkoseksi.

Lisää ensin `heck` `Cargo.toml`-tiedostoosi:

```toml
[dependencies]
heck = "0.4.0"
```

Käytä sitten sitä merkkijonosi suuraakkostamiseen:

```rust
extern crate heck; // Ei tarvita Rustin 2018 editionissa tai myöhemmissä
use heck::TitleCase;

fn main() {
    let my_string = "hello world";
    let capitalized = my_string.to_title_case();
    println!("{}", capitalized); // Tuloste: Hello World
}
```

Huomio: `heck`-paketin tarjoama `to_title_case`-metodi suuraakkostaa jokaisen sanan merkkijonossa, mikä saattaa olla enemmän kuin mitä etsit, jos haluat vain merkkijonon ensimmäisen merkin suuraakkoseksi. Säädä käyttöäsi tarpeidesi mukaan.
