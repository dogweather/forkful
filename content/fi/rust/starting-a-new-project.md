---
title:    "Rust: Uuden projektin aloittaminen"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Miksi aloittaa uusi projekti Rust-ohjelmoinnilla?

Rust on suosittu ohjelmointikieli, joka on suunniteltu erityisesti turvallisuuden ja suorituskyvyn kannalta. Se on myös avoimen lähdekoodin ja kehittäjäyhteisönsä ansiosta saanut paljon huomiota viime vuosina. Joten, miksi aloittaa uusi projekti Rust-ohjelmoinnilla?

Voit luoda turvallisia ja nopeita ohjelmia - Rustilla on sisäänrakennettuja turvallisuustoimintoja ja sen suorituskyky on verrattavissa muihin matalan tason ohjelmointikieliin kuten C:hen. Näin voit välttää muistiongelmia ja optimoida ohjelmasi suorituskykyä.

# Miten aloittaa Rust-projekti?

Ensinnäkin, varmista että Rust Compiler on asennettuna. Voit tarkistaa sen komennolla ```Rust --version```. Seuraavaksi, voit luoda uuden projektin komennolla ```cargo new projektin_nimi```.

Tässä on esimerkkikoodi, joka tulostaa "Hei maailma!" Rustilla:

```
fn main() {
    println!("Hei maailma!");
}
```

Suorita tämä koodi komennolla ```cargo run``` ja näet tuloksen: ```Hei maailma!```

# Syvällisempi sukellus uuden projektin aloittamiseen

Kun luot uuden projektin Rustilla, se luo automaattisesti muutaman tiedoston ja kansion projektin juurihakemiston sisään. Tässä on lyhyt selitys kustakin tiedostosta:

- ```Cargo.toml``` - tässä määritellään projektin riippuvuudet ja muut asetukset.
- ```src``` - kansio, johon voit luoda moduuleja ja kirjoittaa koodia.
- ```src/main.rs``` - tiedosto, jossa on pääohjelma.
- ```target``` - kansio, johon tallennetaan käännöksen lopputulos.

Voit myös lisätä tarvittavia riippuvuuksia ```Cargo.toml```-tiedostoon ja kääntää projektin uudelleen komennolla ```cargo build```.

Kehittäjäyhteisö on myös yksi Rustin vahvuuksista - siellä on saatavilla paljon dokumentaatiota, opetusmateriaaleja ja keskustelupalstoja, jossa voit kysyä apua ja jakaa tietoa muiden kanssa.

# Katso myös

- [Rustin viralliset kotisivut](https://www.rust-lang.org/)
- [Rustin dokumentaatio](https://www.rust-lang.org/learn)
- [Rust-ohjelmointikielen esittely (video, englanniksi)](https://www.youtube.com/watch?v=zF34dRivLOw)

Kiitos lukemisesta ja onnea uuden Rust-projektin aloittamiseen!