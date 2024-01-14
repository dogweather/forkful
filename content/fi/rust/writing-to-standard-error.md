---
title:                "Rust: Kirjoittaminen standardivirheeseen"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi kirjoittaa standardivirheeseen

Rust on yhä suositumpi ohjelmointikieli, joka tarjoaa korkean suorituskyvyn ja turvallisen kehitysympäristön. Yksi tärkeä ominaisuus Rustissa on kyky kirjoittaa ohjelmaan standardivirheeseen (stderr), jota käytetään ohjelman virheiden tulostamiseen. Tässä blogikirjoituksessa käymme läpi, miksi ja miten kirjoittaa standardivirheeseen Rust-ohjelmassa.

## Miten kirjoittaa standardivirheeseen

Rustissa standardivirheeseen kirjoittaminen voidaan tehdä käyttämällä `println!` makroa ja välittämällä sille parametri `eprintln!` sijaan. Tämä varmistaa, että tuloste ohjataan standardivirheeseen eikä standarditulostukseen. Alla on esimerkki koodista:

```Rust
fn main() {
  // Kirjoitetaan standardivirheeseen käyttämällä eprintln! makroa
  eprintln!("Tämä on virheilmoitus!");
}
```

Esimerkin koodi tulostaisi "Tämä on virheilmoitus!" standardivirheeseen. Huomaa, että se ei tulostaisi mitään näytölle tai standarditulostukseen.

## Syvällinen sukellus

Kirjoittaminen standardivirheeseen on hyödyllistä silloin, kun halutaan erottaa ohjelman virheilmoitukset tavallisista tulosteista. Tämä antaa mahdollisuuden selkeästi erottaa virheet ja helpottaa niiden havaitsemista ja käsittelyä. Standardivirheeseen kirjoittaminen on myös hyödyllistä silloin, kun halutaan suorittaa ohjelmaa esimerkiksi komentoriviltä, ja halutaan erottaa tulosteet ja virheilmoitukset.

Rustissa standardivirheeseen voi myös kirjoittaa käyttämällä `eprint!` makroa, joka toimii samalla tavalla kuin `eprintln!` mutta ei lisää uutta riviä loppuun. Tämä voi olla hyödyllistä, jos halutaan tulostaa useita virheilmoituksia peräkkäin samaan riviin.

## Katso myös

Tässä blogikirjoituksessa kävimme läpi, miksi ja miten kirjoittaa standardivirheeseen Rust-ohjelmassa. Jos haluat tutustua lisää Rustin ominaisuuksiin ja kehitystyökaluihin, suosittelemme seuraavia linkkejä:

- Rust-ohjelmointikielen virallinen verkkosivusto: https://www.rust-lang.org/fi
- Rust-oppimateriaalit: https://www.rust-lang.org/learn
- Visual Studio Coden Rust-laajennus: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust